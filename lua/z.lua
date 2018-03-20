--=====================================================================
--
-- zm.lua - z.sh implementation for lua
--
-- Created by skywind on 2018/03/19
-- Last Modified: 2018/03/19 11:11:47
--
--=====================================================================


-----------------------------------------------------------------------
-- Module Header
-----------------------------------------------------------------------
local modname = 'z'
local M = {}
_G[modname] = M
package.loaded[modname] = M  --return modname
setmetatable(M,{__index = _G})

if _ENV ~= nil then
	_ENV[modname] = M
else
	setfenv(1, M)
end


-----------------------------------------------------------------------
-- Environment
-----------------------------------------------------------------------
local windows = package.config:sub(1, 1) ~= '/' and true or false
local in_module = pcall(debug.getlocal, 4, 1) and true or false


-----------------------------------------------------------------------
-- Global Variable
-----------------------------------------------------------------------
MAX_AGE = 5000
DATA_FILE = '~/.zm'
PRINT_TO_STDERR = false
PWD = ''
Z_METHOD = 'frecent'
Z_SUBDIR = false


-----------------------------------------------------------------------
-- split string
-----------------------------------------------------------------------
function string:split(sSeparator, nMax, bRegexp)
	assert(sSeparator ~= '')
	assert(nMax == nil or nMax >= 1)
	local aRecord = {}
	if self:len() > 0 then
		local bPlain = not bRegexp
		nMax = nMax or -1
		local nField, nStart = 1, 1
		local nFirst,nLast = self:find(sSeparator, nStart, bPlain)
		while nFirst and nMax ~= 0 do
			aRecord[nField] = self:sub(nStart, nFirst-1)
			nField = nField+1
			nStart = nLast+1
			nFirst,nLast = self:find(sSeparator, nStart, bPlain)
			nMax = nMax-1
		end
		aRecord[nField] = self:sub(nStart)
	end
	return aRecord
end


-----------------------------------------------------------------------
-- print table
-----------------------------------------------------------------------
function dump(o)
	if type(o) == 'table' then
		local s = '{ '
		for k,v in pairs(o) do
			if type(k) ~= 'number' then k = '"'..k..'"' end
			s = s .. '['..k..'] = ' .. dump(v) .. ','
		end
		return s .. '} '
	else
		return tostring(o)
	end
end


-----------------------------------------------------------------------
-- print table
-----------------------------------------------------------------------
function printT(table, level)
	key = ""
	local func = function(table, level)end
	func = function(table, level)
		level = level or 1
		local indent = ""
		for i = 1, level do
			indent = indent.."  "
		end

		if key ~= "" then
			print(indent..key.." ".."=".." ".."{")
		else
			print(indent .. "{")
		end

		key = ""
		for k, v in pairs(table) do
			if type(v) == "table" then
				key = k
				func(v, level + 1)
			else
				local content = string.format("%s%s = %s", indent .. "  ",tostring(k), tostring(v))
				print(content)  
			end
		end
		print(indent .. "}")
	end
	func(table, level)
end


-----------------------------------------------------------------------
-- get current path
-----------------------------------------------------------------------
function os_pwd()
	if windows then
		local fp = io.popen('cd')
		if fp == nil then
			return ''
		end
		local line = fp:read('l')
		fp:close()
		return line
	else
		local fp = io.popen('pwd')
		if fp == nil then
			return ''
		end
		local line = fp:read('l')
		fp:close()
		return line
	end
end


-----------------------------------------------------------------------
-- dir exists
-----------------------------------------------------------------------
function path_isdir(pathname)
	local name = pathname .. '/'
	local ok, err, code = os.rename(name, name)
	if not ok then
		if code == 13 then
			return true
		end
		return false
	end
	return true
end


-----------------------------------------------------------------------
-- is absolute path
-----------------------------------------------------------------------
function path_isabs(pathname)
	local h1 = pathname:sub(1, 1)
	if windows then
		local h2 = pathname:sub(2, 2)
		local h3 = pathname:sub(3, 3)
		if h1 == '/' or h1 == '\\' then
			return true
		end
		if h2 == ':' and (h3 == '/' or h3 == '\\') then
			return true
		end
	elseif h1 == '/' then
		return true
	end
	return false
end


-----------------------------------------------------------------------
-- normalize path
-----------------------------------------------------------------------
function path_norm(pathname)
	if windows then 
		pathname = pathname:gsub('\\', '/')
	end
	if windows then
		pathname = pathname:gsub('/', '\\')
	end
	return pathname
end


-----------------------------------------------------------------------
-- check subdir
-----------------------------------------------------------------------
function path_subdir(basename, subname)
	if windows then
		basename = basename:gsub('\\', '/')
		subname = subname:gsub('\\', '/')
		basename = basename:lower()
		subname = subname:lower()
	end
	local last = basename:sub(-1, -1)
	if last ~= '/' then
		basename = basename .. '/'
	end
	if subname:find(basename, 0, true) == 1 then
		return true
	end
	return false
end


-----------------------------------------------------------------------
-- expand user home
-----------------------------------------------------------------------
function path_expand(pathname)
	if not pathname:find('~') then
		return pathname
	end
	local home = ''
	if windows then
		home = os.getenv('USERPROFILE')
	else
		home = os.getenv('HOME')
	end
	if pathname == '~' then
		return home
	end
	local head = pathname:sub(1, 2)
	if windows then
		if head == '~/' or head == '~\\' then
			return home .. '\\' .. pathname:sub(3, -1)
		end
	elseif head == '~/' then
		return home .. '/' .. pathname:sub(3, -1)
	end
	return pathname
end


-----------------------------------------------------------------------
-- returns true for path is insensitive
-----------------------------------------------------------------------
function path_case_insensitive()
	if windows then 
		return true
	end
	local eos = os.getenv('OS')
	eos = eos ~= nil and eos or ''
	eos = eos:lower()
	if eos:sub(1, 7) == 'windows' then
		return true
	end
	return false
end


-----------------------------------------------------------------------
-- load and split data
-----------------------------------------------------------------------
function data_load(filename)
	local M = {}
	fp = io.open(path_expand(filename), 'r')
	if fp == nil then
		return nil
	end
	for line in fp:lines() do
		local part = string.split(line, '|')
		local item = {}
		if part and part[1] and part[2] and part[3] then
			item.name = part[1]
			item.rank = tonumber(part[2])
			item.time = tonumber(part[3]) + 0
			item.frecent = item.rank
			if string.len(part[3]) < 12 then
				if item.rank ~= nil and item.time ~= nil then
					table.insert(M, item)
				end
			end
		end
	end
	fp:close()
	return M
end


-----------------------------------------------------------------------
-- save data
-----------------------------------------------------------------------
function data_save(filename, M)
	local fp = nil
	local tmpname = nil
	local i
	filename = path_expand(filename)
	if windows then
		fp = io.open(filename, 'w')
	else
		tmpname = os.tmpname()
		fp = io.open(tmpname, 'w')
	end
	if fp == nil then
		return false
	end
	for i = 1, #M do
		local item = M[i]
		local text = item.name .. '|' .. item.rank .. '|' .. item.time
		fp:write(text .. '\n')
	end
	fp:close()
	if tmpname ~= nil then
		os.rename(tmpname, filename)
	end
	return true
end


-----------------------------------------------------------------------
-- filter out bad dirname
-----------------------------------------------------------------------
function data_filter(M)
	local N = {}
	local i
	M = M ~= nil and M or {}
	for i = 1, #M do
		local item = M[i]
		if path_isdir(item.name) then
			table.insert(N, item)
		end
	end
	return N
end


-----------------------------------------------------------------------
-- insert item
-----------------------------------------------------------------------
function data_insert(M, filename)
	local i = 1
	local sumscore = 0
	for i = 1, #M do 
		local item = M[i]
		sumscore = sumscore + item.rank
	end
	if sumscore >= MAX_AGE then
		local X = {}
		for i = 1, #M do
			local item = M[i]
			item.rank = item.rank * 0.9
			if item.rank >= 1.0 then
				table.insert(X, item)
			end
		end
	end
	local nocase = path_case_insensitive()
	local name = filename
	local key = nocase and string.lower(name) or name
	local find = false
	local current = os.time()
	for i = 1, #M do
		local item = M[i]
		if not nocase then
			if name == item.name then
				item.rank = item.rank + 1
				item.time = current
				find = true
				break
			end
		else
			if key == string.lower(item.name) then
				item.rank = item.rank + 1
				item.time = current
				find = true
				break
			end
		end
	end
	if not find then
		local item = {}
		item.name = name
		item.rank = 1
		item.time = current
		item.frecent = item.rank
		table.insert(M, item)
	end
	return M
end


-----------------------------------------------------------------------
-- change pattern
-----------------------------------------------------------------------
function case_insensitive_pattern(pattern)
	-- find an optional '%' (group 1) followed by any character (group 2)
	local p = pattern:gsub("(%%?)(.)", function(percent, letter)

		if percent ~= "" or not letter:match("%a") then
			-- if the '%' matched, or `letter` is not a letter, return "as is"
			return percent .. letter
		else
			-- else, return a case-insensitive character class of the matched letter
			return string.format("[%s%s]", letter:lower(), letter:upper())
		end
	end)
	return p
end


-----------------------------------------------------------------------
-- pathmatch
-----------------------------------------------------------------------
function path_match(pathname, patterns)
	local pos = 1
	local i = 0
	for i = 1, #patterns do
		local pat = patterns[i]
		start, endup = pathname:find(pat, pos)
		if start == nil or endup == nil then
			return false
		end
		pos = endup + 1
	end
	return true
end


-----------------------------------------------------------------------
-- select matched pathnames
-----------------------------------------------------------------------
function data_select(M, patterns)
	local N = {}
	local i = 1
	local pats = {}
	for i = 1, #patterns do
		local p = patterns[i]
		table.insert(pats, case_insensitive_pattern(p))
	end
	for i = 1, #M do
		local item = M[i]
		if path_match(item.name, pats) then
			table.insert(N, item)
		end
	end
	return N
end


-----------------------------------------------------------------------
-- update frecent
-----------------------------------------------------------------------
function data_update_frecent(M)
	local current = os.time()
	local i
	for i = 1, #M do
		local item = M[i]
		local dx = current - item.time
		if dx < 3600 then 
			item.frecent = item.rank * 4
		elseif dx < 86400 then
			item.frecent = item.rank * 2
		elseif dx < 604800 then
			item.frecent = item.rank * 0.5
		else
			item.frecent = item.rank * 0.25
		end
	end
	return M
end


-----------------------------------------------------------------------
-- add path
-----------------------------------------------------------------------
function z_add(path)
	if not path_isdir(path) then
		return false
	end
	if not path_isabs(path) then
		return false
	end
	local M = data_load(DATA_FILE)
	M = data_filter(M)
	path = path_norm(path)
	M = data_insert(M, path)
	data_save(DATA_FILE, M)
	return true
end


-----------------------------------------------------------------------
-- match method: frecent, rank, time
-----------------------------------------------------------------------
function z_match(patterns, method, subdir)
	patterns = patterns ~= nil and patterns or {}
	method = method ~= nil and method or 'frecent'
	subdir = subdir ~= nil and subdir or false
	local M = data_load(DATA_FILE)
	M = data_select(M, patterns)
	M = data_filter(M)
	M = data_update_frecent(M)
	if method == 'time' then
		current = os.time()
		for _, item in pairs(M) do
			item.score = item.time - current
		end
	elseif method == 'rank' then
		for _, item in pairs(M) do
			item.score = item.rank
		end
	else
		for _, item in pairs(M) do
			item.score = item.frecent
		end
	end
	table.sort(M, function (a, b) return a.score > b.score end)
	if subdir then
		local pwd = (PWD == nil or PWD == '') and os.getenv('PWD') or PWD
		if pwd ~= '' and pwd ~= nil then 
			local N = {}
			for _, item in pairs(M) do
				if path_subdir(pwd, item.name) then
					table.insert(N, item)
				end
			end
			M = N
		end
	end
	return M
end


-----------------------------------------------------------------------
-- pretty print 
-----------------------------------------------------------------------
function z_print(M, number)
	local N = {}
	local maxsize = 10
	local numsize = string.len(tostring(#M))
	for _, item in pairs(M) do
		local record = {}
		record.score = string.format('%.2f', item.score)
		record.name = item.name
		table.insert(N, record)
		if record.score:len() > maxsize then
			maxsize = record.score:len()
		end
	end
	for i = #N, 1, -1 do
		local record = N[i]
		local line = record.score
		local dx = maxsize - line:len()
		if dx > 0 then
			line = line .. string.rep(' ', dx)
		end
		line = line .. '  ' .. record.name
		if number then
			local head = tostring(i)
			if head:len() < numsize then
				head = string.rep(' ', numsize - head:len()) .. head
			end
			line = head .. ':  ' .. line
		end
		if not PRINT_TO_STDERR then
			print(line)
		else
			io.stderr:write(line .. '\n')
		end
	end
end


-----------------------------------------------------------------------
-- calculate jump dir
-----------------------------------------------------------------------
function z_cd(patterns)
	if patterns == nil then
		return nil
	end
	if #patterns == 0 then
		return nil
	end
	local last = patterns[#patterns]
	if path_isdir(last) then
		return last
	end
	local M = z_match(patterns, Z_METHOD, Z_SUBDIR)
	if M == nil then
		return nil
	end
	if #M == 0 then
		return nil
	end
	return M[1].name
end


-----------------------------------------------------------------------
-- main entry
-----------------------------------------------------------------------
function main()
	print('fuck')
end


-----------------------------------------------------------------------
-- testing case
-----------------------------------------------------------------------
if not pcall(debug.getlocal, 4, 1) then
	-- main script
	main()
end


