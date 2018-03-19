--=====================================================================
--
-- zm.lua - z module for lua
--
-- Created by skywind on 2018/03/19
-- Last Modified: 2018/03/19 11:11:47
--
--=====================================================================

local string = string
local table = table
local io = io
local os = os
local package = package
local print = print
local assert = assert
local type = type
local pairs = pairs
local tostring = tostring
local tonumber = tonumber
local windows = package.config:sub(1, 1) ~= '/' and true or false

module('zlua')


-----------------------------------------------------------------------
-- Global Variable
-----------------------------------------------------------------------
local MAX_AGE = 5000


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
		for k,v in pairs(table) do
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
-- load and split data
-----------------------------------------------------------------------
function data_load(filename)
	local M = {}
	fp = io.open(filename, 'r')
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
			item.score = item.rank
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
	local name = filename
	local key = windows and string.lower(name) or name
	local find = false
	local current = os.time()
	for i = 1, #M do
		local item = M[i]
		if not windows then
			if name == item.name then
				item.rank = item.rank + 1
				item.time = current
				find = 1
				break
			end
		else
			if key == string.lower(item.name) then
				item.rank = item.rank + 1
				item.time = current
				find = 1
				break
			end
		end
	end
	if not find then
		local item = {}
		item.name = name
		item.rank = 1
		item.time = current
		item.score = item.rank
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
-- select available paths
-----------------------------------------------------------------------
function data_select(M, patterns, nocase)
	local N = {}
	local i = 1
	local pats = {}
	for i = 1, #patterns do
		local p = patterns[i]
		if not nocase then
			table.insert(pats, p)
		else
			table.insert(pats, case_insensitive_pattern(p))
		end
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
-- testing case
-----------------------------------------------------------------------
local inname = windows and 'c:/users/linwei/.fasd' or '/cygdrive/c/users/linwei/.fasd'
local outname = windows and 'd:/fasd.txt' or '/cygdrive/d/fasd.txt'
local x = data_load(inname)
x = data_filter(x)
x = data_insert(x, 'd:/software')
printT(x)
data_save(outname, x)

print(path_isdir('c:/windows'))
print('---------')
p = 'd:/dev/python27/lib/site-packages'

print(path_match(p, {'lib', 'site'}))

printT(data_select(x, {'i', 'c'}, true))


