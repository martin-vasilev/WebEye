from flask import Flask, render_template, Response
import cv2

app = Flask(__name__)

# Initialize webcam
cap = cv2.VideoCapture(0)

def detect_eyes(frame):
    # Add your eye detection code here
    # This function should take a frame as input and return the frame with eye detection overlays

    return frame

def generate_frames():
    while True:
        # Capture frame-by-frame
        ret, frame = cap.read()

        if not ret:
            break

        # Perform eye detection
        frame_with_eyes = detect_eyes(frame)

        # Encode frame as JPEG
        ret, buffer = cv2.imencode('.jpg', frame_with_eyes)
        frame_bytes = buffer.tobytes()

        yield (b'--frame\r\n'
               b'Content-Type: image/jpeg\r\n\r\n' + frame_bytes + b'\r\n')

@app.route('/')
def index():
    return render_template('index.html')

@app.route('/video_feed')
def video_feed():
    return Response(generate_frames(), mimetype='multipart/x-mixed-replace; boundary=frame')

if __name__ == '__main__':
    app.run(debug=True)
