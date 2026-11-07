import argparse
import http.client
import json
import os
import socket
import sys
import time
from pathlib import Path

SOCK = Path(os.environ["XDG_RUNTIME_DIR"]) / "caddy" / "admin.sock"
STATE_HOME = os.environ.get("XDG_STATE_HOME") or Path.home() / ".local" / "state"
STATE = Path(STATE_HOME) / "caddy-routes"
ROUTES = "/config/apps/http/servers/srv0/routes"


class UnixConnection(http.client.HTTPConnection):
    def __init__(self, path):
        super().__init__("caddy")
        self.path = path

    def connect(self):
        self.sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        self.sock.connect(str(self.path))


class APIError(Exception):
    def __init__(self, status, body):
        super().__init__(f"caddy admin API: {status} {body.strip()}")
        self.status = status


def api(method, path, body=None):
    conn = UnixConnection(SOCK)
    try:
        payload = None if body is None else json.dumps(body)
        headers = {"Content-Type": "application/json"} if payload else {}
        conn.request(method, path, payload, headers)
        resp = conn.getresponse()
        data = resp.read().decode()
    finally:
        conn.close()
    if resp.status >= 400:
        raise APIError(resp.status, data)
    return json.loads(data) if data.strip() else None


def route_id(host):
    return "route:" + host


def build_route(host, upstream, allow):
    routes = []
    if allow:
        routes.append(
            {
                "match": [{"not": [{"remote_ip": {"ranges": allow}}]}],
                "handle": [
                    {
                        "handler": "static_response",
                        "status_code": 403,
                        "body": "Forbidden",
                    }
                ],
                "terminal": True,
            }
        )
    routes.append(
        {
            "handle": [
                {"handler": "reverse_proxy", "upstreams": [{"dial": upstream}]}
            ]
        }
    )
    return {
        "@id": route_id(host),
        "match": [{"host": [host]}],
        "handle": [{"handler": "subroute", "routes": routes}],
        "terminal": True,
    }


def remove(host):
    try:
        api("DELETE", "/id/" + route_id(host))
    except APIError as err:
        if err.status != 404:
            raise


def apply(host, route):
    remove(host)
    api("PUT", ROUTES + "/0", route)


def cmd_add(args):
    route = build_route(args.host, args.upstream, args.allow)
    STATE.mkdir(parents=True, exist_ok=True)
    tmp = STATE / (args.host + ".json.tmp")
    tmp.write_text(json.dumps(route, indent=2) + "\n")
    tmp.replace(STATE / (args.host + ".json"))
    apply(args.host, route)


def cmd_rm(args):
    (STATE / (args.host + ".json")).unlink(missing_ok=True)
    remove(args.host)


def handlers(route, kind):
    for handler in route.get("handle", []):
        if handler.get("handler") == kind:
            yield handler
        for sub in handler.get("routes", []):
            yield from handlers(sub, kind)


def cmd_ls(args):
    for route in api("GET", ROUTES) or []:
        if not route.get("@id", "").startswith("route:"):
            continue
        host = route["match"][0]["host"][0]
        upstream = next(handlers(route, "reverse_proxy"))["upstreams"][0]["dial"]
        allow = [
            cidr
            for sub in route["handle"][0]["routes"]
            for match in sub.get("match", [])
            for neg in match.get("not", [])
            for cidr in neg.get("remote_ip", {}).get("ranges", [])
        ]
        print("\t".join([host, upstream, ",".join(allow)]))


def cmd_restore(args):
    for _ in range(50):
        try:
            api("GET", "/config/apps/http/servers/srv0")
            break
        except (OSError, APIError):
            time.sleep(0.2)
    for path in sorted(STATE.glob("*.json")):
        apply(path.name.removesuffix(".json"), json.loads(path.read_text()))


def main():
    parser = argparse.ArgumentParser(
        prog="caddy-route",
        description="Register sites with the user Caddy at runtime.",
    )
    sub = parser.add_subparsers(dest="command", required=True)

    add = sub.add_parser("add", help="add or replace a site")
    add.add_argument("host")
    add.add_argument("upstream", help="host:port to proxy to")
    add.add_argument(
        "--allow",
        action="append",
        default=[],
        metavar="CIDR",
        help="client address or range let through (repeatable)",
    )
    add.set_defaults(func=cmd_add)

    rm = sub.add_parser("rm", help="remove a site")
    rm.add_argument("host")
    rm.set_defaults(func=cmd_rm)

    sub.add_parser("ls", help="list sites").set_defaults(func=cmd_ls)
    sub.add_parser(
        "restore", help="re-apply saved sites (run when Caddy starts)"
    ).set_defaults(func=cmd_restore)

    args = parser.parse_args()
    try:
        args.func(args)
    except (OSError, APIError) as err:
        print(f"caddy-route: {err}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
