#! /usr/bin/env python
# -*- coding: utf-8 -*-
#======================================================================
#
# console.py - 
#
# Created by skywind on 2018/01/10
# Last Modified: 2023/01/10 07:30:58
#
#======================================================================
from __future__ import unicode_literals, print_function
import sys
import time
import os


#----------------------------------------------------------------------
# exports
#----------------------------------------------------------------------
__all__ = [ 'log', 'enable', 'disable', 'set_level', 'set_color', 
            'cprint', 'PALETTE' ]


#----------------------------------------------------------------------
# color
#----------------------------------------------------------------------
def __set_console_color(color):
    if sys.platform[:3] == 'win':
        import ctypes
        kernel32 = ctypes.windll.LoadLibrary('kernel32.dll')
        GetStdHandle = kernel32.GetStdHandle
        SetConsoleTextAttribute = kernel32.SetConsoleTextAttribute
        GetStdHandle.argtypes = [ ctypes.c_uint32 ]
        GetStdHandle.restype = ctypes.c_size_t
        SetConsoleTextAttribute.argtypes = [ ctypes.c_size_t, ctypes.c_uint16 ]
        SetConsoleTextAttribute.restype = ctypes.c_long
        handle = GetStdHandle(0xfffffff5)
        if color < 0: color = 7
        result = 0
        if (color & 1): result |= 4
        if (color & 2): result |= 2
        if (color & 4): result |= 1
        if (color & 8): result |= 8
        if (color & 16): result |= 64
        if (color & 32): result |= 32
        if (color & 64): result |= 16
        if (color & 128): result |= 128
        SetConsoleTextAttribute(handle, result)
    else:
        if color >= 0:
            foreground = color & 7
            background = (color >> 4) & 7
            bold = color & 8
            if background == 0:
                t = "\033[%s3%d"%(bold and "01;" or "", foreground)
            else:
                t = "\033[%s3%d;4%dm"%(bold and "01;" or "", foreground, background)
            sys.stdout.write(t)
            sys.stdout.flush()
        else:
            sys.stdout.write("\033[0m")
            sys.stdout.flush()
    return 0


#----------------------------------------------------------------------
# LEVEL: 0/disable, 1/channel, 2/all
#----------------------------------------------------------------------
LOG_LEVEL = 2                     # 0/disable, 1/channel, 2/all
LOG_FILE = None                   # set to fp to write file

__channel_mask = {}
__channel_color = { }

__has_tty = os.isatty(sys.stdout.fileno())


#----------------------------------------------------------------------
# color palette (16 colors)
#----------------------------------------------------------------------
PALETTE = {
            'BLACK': 0, 'RED': 1, 'GREEN': 2, 'YELLOW': 3, 'BLUE': 4,
            'MAGENTA': 5, 'CYAN': 6, 'WHITE': 7, 'BOLD': 8,
            'BOLD_RED': 9, 'BOLD_GREEN': 10, 'BOLD_YELLOW': 11, 
            'BOLD_BLUE': 12, 'BOLD_MAGENTA': 13, 'BOLD_CYAN': 14,
            'BOLD_WHITE': 15, 'MAROON': 1, 'OLIVER': 3, 'NAVY': 4, 
            'PURPLE': 5, 'TEAL': 6, 'SILVER': 7, 'GREY': 8, 'LIME': 10,
            'FUCHSIA': 13, 'AQUA': 14, 'LIGHT_RED': 9, 'LIGHT_GREEN': 10, 
            'LIGHT_YELLOW': 11, 'LIGHT_BLUE': 12, 'LIGHT_MAGENTA': 13, 
            'LIGHT_CYAN': 14, 'LIGHT_WHITE': 15, 'HIGHLIGHT': 15,
        }



#----------------------------------------------------------------------
# cprint
#----------------------------------------------------------------------
def cprint(color, *args):
    if not __has_tty:
        color = None
    if color is not None:
        __set_console_color(color)
    text = ' '.join([str(n) for n in args])
    sys.stdout.write(text + '\n')
    if color is not None:
        __set_console_color(-1)
    return 0


#----------------------------------------------------------------------
# enable certain channel
#----------------------------------------------------------------------
def enable(*channel):
    for ch in channel:
        __channel_mask[ch] = True
    return 0


#----------------------------------------------------------------------
# disable certain channel 
#----------------------------------------------------------------------
def disable(*channel):
    for ch in channel:
        __channel_mask[ch] = False
    return 0


#----------------------------------------------------------------------
# change log level
#----------------------------------------------------------------------
def set_level(level):
    global LOG_LEVEL
    assert level >= 0 and level < 3
    LOG_LEVEL = level
    return 0


#----------------------------------------------------------------------
# change channel color
#----------------------------------------------------------------------
def set_color(channel, color):
    if isinstance(color, str):
        key = color.upper()
        if key not in PALETTE:
            raise ValueError('invalid color name: ' + color)
        color = PALETTE[key]
    __channel_color[channel] = color
    return 0


#----------------------------------------------------------------------
# write log
#----------------------------------------------------------------------
def log(channel, *args):
    if LOG_LEVEL <= 0:
        return 0
    elif LOG_LEVEL == 1:
        if not __channel_mask.get(channel, False):
            return 0
    color = __channel_color.get(channel, None)
    text = ' '.join([str(n) for n in args])
    if __has_tty:
        cprint(color, text)
    else:
        print(text)
    if LOG_FILE:
        now = time.strftime('%Y-%m-%d %H:%M:%S')
        t = '[%s] %s'%(now, text)
        LOG_FILE.write(t + '\n')
    return 1


#----------------------------------------------------------------------
# get log function for one channel
#----------------------------------------------------------------------
def get(channel, enable = True):
    def output(*args):
        if not enable: return -1
        return log(channel, *args)
    return output


#----------------------------------------------------------------------
# example: with scope_log('INFO') as clog: xxx
#----------------------------------------------------------------------
class scope_log (object):
    def __init__ (self, channel, enable = True):
        self.channel = channel
        self.enable = enable
    def __enter__ (self):
        return self
    def __exit__ (self, vtype, value, trace):
        return None
    def __call__ (self, *args):
        if not self.enable: return -1
        return log(self.channel, *args)


#----------------------------------------------------------------------
# testing suit
#----------------------------------------------------------------------
if __name__ == '__main__':
    def test1():
        cprint(15, 'hahaha')
        return 0
    def test2():
        set_level(1)
        enable(1, 2, 5, 6, 7)
        set_color(5, 15)
        set_color(6, 12)
        log(1, 'channel 1')
        log(5, 'channel 5')
        log(6, 'channel 6')
        log(7, 'channel 1')
    def test3():
        set_color(5, 15)
        set_color(6, 12)
        log1 = get(5, True)
        log2 = get(6, True)
        log1('haha1')
        log2('haha2')
        log1('foo1')
        log2('bar1')
    def test4():
        set_color(5, 'LIGHT_GREEN')
        set_color(6, 'LIGHT_YELLOW')
        with scope_log(5) as clog:
            clog('hello', 'world')
        with scope_log(6) as clog:
            clog('hello', 'world')
    test3()


