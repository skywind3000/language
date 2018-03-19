--=====================================================================
--
-- zm.lua - z module for lua
--
-- Created by skywind on 2018/03/19
-- Last Modified: 2018/03/19 11:11:47
--
--=====================================================================
local print = print
module('zlua')

function data_load(filename)
	print('filename: '..filename)
end

data_load('abc.txt')
