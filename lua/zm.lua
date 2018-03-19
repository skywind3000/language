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
function isdir(pathname)
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
-- load and split data
-----------------------------------------------------------------------
function data_load(filename)
	local M = {}
	for line in io.lines(filename) do
		local part = string.split(line, '|')
		local item = {}
		if part and part[1] and part[2] and part[3] then
			item.name = part[1]
			item.rank = tonumber(part[2])
			item.time = tonumber(part[3]) + 0
			if string.len(part[3]) < 12 then
				table.insert(M, item)
			end
		end
	end
	return M
end


-----------------------------------------------------------------------
-- save data
-----------------------------------------------------------------------
function data_save(filename, M)
	local fp = nil
	local tmpname = nil
	if windows then
		fp = io.open(filename, 'w')
	else
		tmpname = os.tmpname()
		fp = io.open(tmpname, 'w')
	end
	local i = 1
	while true do
		local item = M[i]
		if item == nil then
			break
		end
		local text = item.name .. '|' .. item.rank .. '|' .. item.time
		fp:write(text .. '\n')
		i = i + 1
	end
	fp:close()
	if tmpname ~= nil then
		os.rename(tmpname, filename)
	end
end


-----------------------------------------------------------------------
-- filter out bad dirname
-----------------------------------------------------------------------
function data_filter(M)
	local N = {}
	local i = 1
	while true do
		local item = M[i]
		if item == nil then 
			break
		end
		if isdir(item.name) then
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
printT(x)
data_save(outname, x)

print(isdir('c:/windows'))
