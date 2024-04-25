from flask import Flask, render_template, Response
import cv2
import numpy as np

app = Flask(__name__)

# Load the pre-trained Haar Cascade for eye detection
right_eye_cascade = cv2.CascadeClassifier(cv2.data.haarcascades + 'haarcascade_righteye_2splits.xml')

# Load the pre-trained Haar Cascade for face detection
face_cascade = cv2.CascadeClassifier(cv2.data.haarcascades + 'haarcascade_frontalface_default.xml')

# Initialize the webcam
cap = cv2.VideoCapture(0)

def detect_eye(frame):
    # Flip the frame horizontally to correct the mirror effect
    frame = cv2.flip(frame, 1)

    # Convert the frame to grayscale for better processing
    gray = cv2.cvtColor(frame, cv2.COLOR_BGR2GRAY)

    # Detect faces in the frame
    faces = face_cascade.detectMultiScale(gray, scaleFactor=1.1, minNeighbors=5, minSize=(30, 30))

    # Process each detected face
    for (x, y, w, h) in faces:
        # Draw rectangle around the face
        cv2.rectangle(frame, (x, y), (x + w, y + h), (255, 0, 0), 2)

        # Extract the region of interest (face)
        face_img = frame[y:y+h, x:x+w]

        # Convert the region of interest to grayscale for eye detection
        face_gray = cv2.cvtColor(face_img, cv2.COLOR_BGR2GRAY)

        # Detect right eye in the face region
        right_eyes = right_eye_cascade.detectMultiScale(face_gray, scaleFactor=1.1, minNeighbors=5, minSize=(30, 30))

        # Process each detected right eye
        for (rex, rey, rew, reh) in right_eyes:
            # Ensure only the right eye is processed
            if x + rex > x + w / 2:
                # Draw rectangle around the right eye
                cv2.rectangle(frame, (x + rex, y + rey), (x + rex + rew, y + rey + reh), (0, 255, 0), 2)

                # Extract the region of interest (right eye)
                right_eye_img = frame[y + rey:y + rey + reh, x + rex:x + rex + rew]

                # Flip the right eye image back to its original orientation
                right_eye_img = cv2.flip(right_eye_img, 1)

                # Zoom in on the right eye
                zoom_factor = 4  # Increase this value to zoom in more
                zoomed_right_eye = cv2.resize(right_eye_img, (0, 0), fx=zoom_factor, fy=zoom_factor)

                # Calculate margins based on zoomed right eye dimensions
                margin_y = int(0.15 * zoomed_right_eye.shape[0])
                margin_x = int(0.15 * zoomed_right_eye.shape[1])

                # Crop the zoomed right eye to include only the eyeball with equal margins around it
                cropped_zoomed_right_eye = zoomed_right_eye[margin_y:-margin_y, margin_x:-margin_x]

                # Display the cropped zoomed-in right eye with crosshair in the top right corner of the frame
                frame[10:10+cropped_zoomed_right_eye.shape[0], frame.shape[1]-10-cropped_zoomed_right_eye.shape[1]:frame.shape[1]-10] = cropped_zoomed_right_eye

    return frame

def generate_frames():
    while True:
        # Capture frame-by-frame
        ret, frame = cap.read()

        if not ret:
            break

        # Perform eye detection
        frame_with_eyes = detect_eye(frame)

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
