# -*- coding: utf-8 -*-
"""
Created on Thu May 23 11:30:26 2024

@author: Martin
"""

#!/usr/bin/python   
# Labvanced Ping-Pong example (send and receive data).

# This server waits to receive a "ping" message from a running labvanced experiment (which contains a name as a value).
# After receiving the "ping" it sends back a "pong" message to the running experiment containing the value "Hello {name}".

# To test this, run this script first and then start the following study on Labvanced https://www.labvanced.com/page/library/51053 

import nest_asyncio
nest_asyncio.apply()
import asyncio
import json
import websockets


IP_ADDRESS = 'localhost'
WEBSOCKET_PORT = 8081


async def on_connect(websocket, path):
    print("websocket connection established")
    try:
        async for message in websocket:
            data = json.loads(message)
            # data has two fields 'msg' and 'data'. msg is the trigger / message, value is the variable value, both
            # defined in the 'send external trigger action' in your Labvanced experiment.
            received_value = data['value']
            received_msg = data['msg']
            
            #return received_msg, received_value

            # here you can make an if-elif for each of your triggers and depending on the type execute different code
            if received_msg == 'ping':
                # at this location you could add some code to send a trigger to an external device
                print("msg = {}, received_value = {}".format(received_msg, received_value))

                # this is how you send messages back to the Labvanced player. Specify a 'msg' field, which can be used
                # as a trigger and optionally send some data in the 'value' field. Depending on your application this
                # code can / should be placed at a different location.
                send_value = f"Hello {received_value}"
                await websocket.send(json.dumps({'msg': 'pong', 'value': send_value}))
            elif received_msg == 'anotherTrigger':
                # do something else here
                print("another trigger was received")
            else:
                # unsupported trigger
                print("unsupported event: {}".format(data))
    finally:
        print("connection lost")
        


# Make sure that the IP address and port match with the Labvanced study settings.
asyncio.get_event_loop().run_until_complete(websockets.serve(on_connect, IP_ADDRESS, WEBSOCKET_PORT))
asyncio.get_event_loop().run_forever()