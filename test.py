from websocket import create_connection
import select
import json

def non_blocking_recv(ws, timeout=1):
    ready = select.select([ws.sock], [], [], timeout)
    if ready[0]:
        try:
            return ws.recv()
        except Exception as e:
            print(f"Error receiving message: {e}")
            return None
    return None

# Establish the WebSocket connection
ws = create_connection("ws://localhost:8081")
print("Connection established")

# package= json.dumps({{"msg": "el_x", "value": 500}})
# ws.send(package)
# a= non_blocking_recv(ws, 1)

combined_message = {
        "msg": 'el_x',
        "value": 500
    }
    
# Convert combined message to JSON
combined_message_json = json.dumps(combined_message)
    
# Send the combined message
ws.send(combined_message_json)N