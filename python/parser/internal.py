#! /usr/bin/env python
# -*- coding: utf-8 -*-
#  vim: set ts=4 sw=4 tw=0 et :
#======================================================================
#
# internal.py - 
#
# Created by skywind on 2015/01/08
# Last Modified: 2023/01/08 00:32:59
#
#======================================================================
import sys
import time
import re
import collections
import cstring
import console


#----------------------------------------------------------------------
# LOG LEVEL
#----------------------------------------------------------------------
__all__ = ['bfs', 'echo_error', 'echo_warning', 'symbol_error', 
           'symbol_warning', 'fatal', 'log_info', 'log_debug']


#----------------------------------------------------------------------
# LOGGING
#----------------------------------------------------------------------
LOG_ERROR = 0
LOG_WARNING = 1
LOG_INFO = 3
LOG_HIGHLIGHT = 4
LOG_DEBUG = 5
LOG_VERBOSE = 6
LOG_GRAMMAR = 10
LOG_LR = 11
LOG_LR_DEBUG = 12
LOG_PDA = 13
LOG_PDA_DEBUG = 14


# color names are defined in console.py
PALETTE = {
            LOG_ERROR: 'LIGHT_RED',
            LOG_WARNING: 'LIGHT_YELLOW',
            LOG_INFO: -1,
            LOG_HIGHLIGHT: 'LIGHT_WHITE',
            LOG_DEBUG: 'GREY',
            LOG_VERBOSE: 'GREY',
            LOG_GRAMMAR: 'WHITE',
            LOG_LR: 'WHITE',
            LOG_LR_DEBUG: 'GREY',
            LOG_PDA: 'LIGHT_CYAN',
            LOG_PDA_DEBUG: 'GREY',
        }

# all log channels are enabled by default
# channel color are defined as well.
for name, value in list(globals().items()):
    if name.startswith('LOG_'):
        __all__.append(name)
        console.set_color(value, PALETTE[value])
        console.enable(value)

console.set_level(1)


#----------------------------------------------------------------------
# echo_error
#----------------------------------------------------------------------
def echo_error(text, fn = None, line_num = 0, col = None):
    name = (not fn) and '<buffer>' or fn
    if not fn:
        t = 'error: %s'%(text, )
    elif (not col) or (col < 0):
        t = 'error:%s:%d: %s'%(name, line_num, text)
    else:
        t = 'error:%s:%d:%d: %s'%(name, line_num, col, text) 
    console.log(LOG_ERROR, t)
    return 0


#----------------------------------------------------------------------
# echo_warning
#----------------------------------------------------------------------
def echo_warning(text, fn = None, line_num = 0, col = None):
    name = (not fn) and '<buffer>' or fn
    if not fn:
        t = 'warning: %s'%(text, )
    elif (not col) or (col < 0):
        t = 'warning:%s:%d: %s'%(name, line_num, text)
    else:
        t = 'warning:%s:%d:%d: %s'%(name, line_num, col, text) 
    console.log(LOG_WARNING, t)
    return 0


#----------------------------------------------------------------------
# echo normal
#----------------------------------------------------------------------
def log_info(*args):
    console.log(LOG_INFO, *args)
    return 0


#----------------------------------------------------------------------
# log debug
#----------------------------------------------------------------------
def log_debug(*args):
    console.log(LOG_DEBUG, *args)
    return 0


#----------------------------------------------------------------------
# fatal
#----------------------------------------------------------------------
def fatal(*args):
    t = ' '.join(args)
    console.cprint(9, 'fatal: ' + t)
    print('abort')
    print()
    sys.exit(1)
    return 0


#----------------------------------------------------------------------
# display symbol error
#----------------------------------------------------------------------
def symbol_error(grammar, symbol, *args):
    text = ' '.join(args)
    fn, line_num = grammar.anchor_get(symbol)
    return echo_error(text, fn, line_num)


#----------------------------------------------------------------------
# display symbol error
#----------------------------------------------------------------------
def symbol_warning(grammar, symbol, *args):
    text = ' '.join(args)
    fn, line_num = grammar.anchor_get(symbol)
    return echo_warning(text, fn, line_num)


#----------------------------------------------------------------------
# symbol set to string
#----------------------------------------------------------------------
def symbol_set_to_string(s):
    t = []
    if len(s) == 0:
        return '{ }'
    for n in s:
        t.append((str(n) == '') and '<empty>' or str(n))
    return '{ %s }'%(', '.join(t),)


#----------------------------------------------------------------------
# error on production
#----------------------------------------------------------------------
def rule_error(grammar, production, *args):
    text = ' '.join(args)
    fn, line_num = grammar.anchor_get(production)
    return echo_error(text, fn, line_num)


#----------------------------------------------------------------------
# warning on production
#----------------------------------------------------------------------
def rule_warning(grammar, production, *args):
    text = ' '.join(args)
    fn, line_num = grammar.anchor_get(production)
    return echo_warning(text, fn, line_num)


#----------------------------------------------------------------------
# bfs generator
#----------------------------------------------------------------------
def bfs(initial, expand):
    open_list = collections.deque(list(initial))
    visited = set(open_list)
    while open_list:
        node = open_list.popleft()
        yield node
        for child in expand(node):
            if child not in visited:
                open_list.append(child)
                visited.add(child)
    return 0


#----------------------------------------------------------------------
# testing suit
#----------------------------------------------------------------------
if __name__ == '__main__':
    def test1():
        echo_error('haha')
        echo_error('haha', '<buffer>', 3, 4)
        fatal('error')
        return 0
    test1()


