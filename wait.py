# -*- coding: utf-8 -*-
"""
Created on Fri Jun  7 13:37:14 2024

@author: Martin
"""

import pygame
import sys
from pygame.locals import *

pygame.init()

def wait_key(key_list, duration=sys.maxsize):
    """ detect and return a keypress, terminate the task if ESCAPE is pressed

    parameters:
    key_list: allowable keys (pygame key constants, e.g., [K_a, K_ESCAPE]
    duration: the maximum time allowed to issue a response (in ms)
              wait for response 'indefinitely' (with sys.maxsize)
    """

    got_key = False
    # clear all cached events if there are any
    pygame.event.clear()
    t_start = pygame.time.get_ticks()
    resp = [None, t_start, -1]

    while not got_key:
        # check for time out
        if (pygame.time.get_ticks() - t_start) > duration:
            break

        # check key presses
        for ev in pygame.event.get():
            if ev.type == KEYDOWN:
                if ev.key in key_list:
                    resp = [pygame.key.name(ev.key),
                            t_start,
                            pygame.time.get_ticks()]
                    got_key = True

            if (ev.type == KEYDOWN) and (ev.key == K_q):
                if ev.mod in [KMOD_LCTRL, KMOD_RCTRL, 4160, 4224]:
                   resp[0]= 'quit'

    # clear the screen following each keyboard response
    #win_surf = pygame.display.get_surface()
    #win_surf.fill(genv.getBackgroundColor())
    #pygame.display.flip()

    return resp

wait_key([K_RETURN])