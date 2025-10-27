local M = {}

function parse_stderr(output)
	local diagnostics = {}

	local lines = vim.split(output, "\n")
	for i, line in ipairs(lines) do
		if line == "" then
			break
		end

		local pattern = ""
		if i == 1 then
			pattern = "%d%d%d%d/%d%d/%d%d %d%d:%d%d:%d%d [^:]+:(%d+):(%d+): (.+)"
		else
			pattern = "[^:]+:(%d+):(%d+): (.+)"
		end
		_, _, lnum_str, col_str, message = string.find(line, pattern)

		lnum = (tonumber(lnum_str) or 1) - 1
		col = (tonumber(col_str) or 1) - 1
		table.insert(diagnostics, {
			lnum = lnum,
			col = col,
			end_lnum = lnum,
			end_col = col,
			severity = vim.diagnostic.severity.ERROR,
			source = "api-linter",
			message = message,
		})
	end

	return diagnostics
end

function parse_stdout(output)
	local diagnostics = {}

	local out = vim.json.decode(output)
	for _, perf in ipairs(out) do
		for _, prob in ipairs(perf.problems) do
			local loc = prob.location
			table.insert(diagnostics, {
				lnum = (loc.start_position.line_number or 1) - 1,
				col = (loc.start_position.column_number or 1) - 1,
				end_lnum = (loc.end_position.line_number or 1) - 1,
				end_col = (loc.end_position.column_number or 1) - 1,
				severity = vim.diagnostic.severity.WARN,
				source = "api-linter",
				message = prob.message,
				code = prob.rule_id,
			})
		end
	end

	return diagnostics
end

function setup_proto_linters()
	local args = {}
	local env_args = os.getenv("API_LINTER_ARGS") or ""
	for arg in env_args:gmatch("%S+") do
		table.insert(args, arg)
	end

	local json_args = { "--output-format", "json" }
	table.move(json_args, 1, #json_args, #args + 1, args)

	require("lint").linters.proto_api_linter = {
		cmd = "api-linter",
		args = args,
		stdin = false,
		append_fname = true,
		stream = "both",
		ignore_exitcode = true,
		parser = function(output)
			if output == "" then
				return {}
			end

			if string.find(output, "^%d%d%d%d/%d%d/%d%d %d%d:%d%d:%d%d") ~= nil then
				return parse_stderr(output)
			else
				return parse_stdout(output)
			end
		end,
	}
end

M.setup = function()
	setup_proto_linters()
end

return M
