#!/usr/bin/env python3
import gi
gi.require_version('Playerctl', '2.0')

from gi.repository import Playerctl, GLib

import os, stat
import subprocess
import sys
import time
import threading

FIFO = '/tmp/.music_xmobar'
PLAYER = 'spotify'
ANIMATE = True
DELAY = 0.7
META_LENGTH = 100

def parse_xresources():
    xrdb = subprocess.run(['xrdb', '-query'], capture_output=True).stdout.decode()
    red = None
    green = None
    yellow = None
    for l in xrdb.split('\n'):
        if '*color1:' in l:
            red = '<fc=' + l.split()[1] + '>'
        if '*color2:' in l:
            green = '<fc=' + l.split()[1] + '>'
        if '*color3:' in l:
            yellow = '<fc=' + l.split()[1] + '>'
    return (red, green, yellow)

def colorize(s, color):
    return color + s + '</fc>'

def make_fifo():
    global FIFO
    print(f'Making fifo {FIFO}')
    os.mkfifo(FIFO, 0o666)

if os.path.exists(FIFO):
    if not stat.S_ISFIFO(os.stat(FIFO).st_mode):
        make_fifo()
else:
    make_fifo()

fifo = open(FIFO, 'w')


CONTROL_FMT = f'<action=playerctl previous -p {PLAYER}> <fn=1>\uf04a</fn> </action><action=playerctl play-pause -p {PLAYER}> %s </action><action=playerctl next -p {PLAYER}> <fn=1>\uf04e</fn> </action>'

red, green, yellow = parse_xresources()

manager = Playerctl.PlayerManager()

lock = threading.Lock()

class Data:
    pass
data = Data()

data.metadata = 'No metadata'
data.control = 'No control'

def animate(data):
    while True:
        with lock:
            put_data(data)
            if(len(data.metadata) > META_LENGTH):
                data.metadata = data.metadata[1:] + data.metadata[0]
        time.sleep(DELAY)

def put_data(data):
    fifo.write((f'<action=~/.xmonad/xmonadctl 13>%-.{META_LENGTH}s</action> %s\n') % (data.metadata, data.control))
    fifo.flush()

def is_playing(player):
    return player.props.status == 'Playing'

def get_metadata(player):
    metadata = player.props.metadata
    keys = metadata.keys()
    if 'xesam:artist' in keys and 'xesam:title' in keys:
        return('{} - {}'.format(metadata['xesam:artist'][0],
                               metadata['xesam:title']))
    return 'No metadata'

def update(player, event, manager):
    with lock:
        if is_playing(player):
            data.control = colorize(CONTROL_FMT % ('<fn=1></fn>'), green)
        else:
            data.control = colorize(CONTROL_FMT % ('<fn=1></fn>'), yellow)
        data.metadata = get_metadata(player)
        put_data(data)
        if len(data.metadata) > META_LENGTH:
            data.metadata += ' | '

def init_player(name):
    # choose if you want to manage the player based on the name
    if name.name in ['spotify']:
        player = Playerctl.Player.new_from_name(name)
        player.connect('playback-status::playing', update, manager)
        player.connect('playback-status::paused', update, manager)
        player.connect('metadata', update, manager)
        manager.manage_player(player)
        update(player, None, None)


def on_name_appeared(manager, name):
    init_player(name)


def on_player_vanished(manager, player):
    fifo.wirte(colorize('Players not found\n', red))
    fifo.flush()


manager.connect('name-appeared', on_name_appeared)
manager.connect('player-vanished', on_player_vanished)

for name in manager.props.player_names:
    init_player(name)

if ANIMATE:
    animate_thread = threading.Thread(target=animate, args=(data,))
    animate_thread.start()

main = GLib.MainLoop()
main.run()
