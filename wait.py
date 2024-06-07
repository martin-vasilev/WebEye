# -*- coding: utf-8 -*-
"""
Created on Fri Jun  7 13:37:14 2024

@author: Martin
"""

def wait_for_key():
    from pynput import keyboard

    result = []
    
    def on_activate_d():
        result.append('drift')
        h.stop()
    
    def on_activate_c():
        result.append('calibrate')
        h.stop()
    
    def quit():
        result.append('quit')
        h.stop()
    
    with keyboard.GlobalHotKeys({
            'd': on_activate_d,
            'c': on_activate_c,
            '<ctrl>+e': quit}) as h:
        h.join()
    
    return result[0] if result else None

# Example usage
#output = wait_for_key()
#print(output)
