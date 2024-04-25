from flask import Flask, render_template, Response
import cv2
import numpy as np

app = Flask(__name__)

# Load the pre-trained Haar Cascade for eye detection
eye_cascade = cv2.CascadeClassifier(cv2.data.haarcascades + 'haarcascade_eye.xml')

# Initialize the webcam
cap = cv2.VideoCapture(0)

def detect_eyes(frame):
    # Flip the frame horizontally to correct the mirror effect
    frame = cv2.flip(frame, 1)

    # Convert the frame to grayscale for better processing
    gray = cv2.cvtColor(frame, cv2.COLOR_BGR2GRAY)

    # Detect eyes in the frame
    eyes = eye_cascade.detectMultiScale(gray, scaleFactor=1.1, minNeighbors=5, minSize=(30, 30))

    # Initialize variables to store the coordinates of the right eye
    right_eye = None
    max_x = 0

    # Loop through the detected eyes to find the right eye (the one with maximum x-coordinate)
    for (x, y, w, h) in eyes:
        if x > max_x:
            max_x = x
            right_eye = (x, y, w, h)

    # Process the right eye if detected
    if right_eye is not None:
        x, y, w, h = right_eye
        # Draw rectangle around the right eye
        cv2.rectangle(frame, (x, y), (x + w, y + h), (255, 0, 0), 2)

        # Extract the region of interest (right eye)
        right_eye_img = frame[y:y+h, x:x+w]

        # Flip the right eye image back to its original orientation
        right_eye_img = cv2.flip(right_eye_img, 1)

        # Zoom in on the right eye
        zoom_factor = 2  # Increase this value to zoom in more
        zoomed_right_eye = cv2.resize(right_eye_img, (0, 0), fx=zoom_factor, fy=zoom_factor)

        # Crop some of the outer margins of the zoomed-in eye to focus more on the eyeball
        margin = int(0.2 * min(zoomed_right_eye.shape[0], zoomed_right_eye.shape[1]))
        cropped_zoomed_right_eye = zoomed_right_eye[margin:-margin, margin:-margin]

        # Find the center of the pupil in the cropped zoomed-in right eye
        right_eye_gray = cv2.cvtColor(cropped_zoomed_right_eye, cv2.COLOR_BGR2GRAY)
        _, right_eye_thresh = cv2.threshold(right_eye_gray, 30, 255, cv2.THRESH_BINARY_INV)
        contours, _ = cv2.findContours(right_eye_thresh, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
        if contours:
            # Find the contour with maximum area (the pupil)
            pupil_contour = max(contours, key=cv2.contourArea)
            # Calculate the centroid of the contour
            M = cv2.moments(pupil_contour)
            if M["m00"] != 0:
                # Calculate the coordinates relative to the cropped zoomed-in eye
                pupil_center_x = int(M["m10"] / M["m00"])
                pupil_center_y = int(M["m01"] / M["m00"])

                # Convert the coordinates of the crosshair to the full resolution of the camera image
                pupil_center_x_full = int((x + pupil_center_x - margin) / zoom_factor)
                pupil_center_y_full = int((y + pupil_center_y - margin) / zoom_factor)

                # Draw a blue crosshair at the center of the pupil in the full camera image
                cv2.drawMarker(frame, (pupil_center_x_full, pupil_center_y_full), (255, 0, 0), cv2.MARKER_CROSS, 10, 2)

                # Display the pixel coordinates of the center of the crosshair relative to the full camera image
                pupil_text = f'Pupil: ({pupil_center_x_full}, {pupil_center_y_full})'
                cv2.putText(frame, pupil_text, (10, frame.shape[0] - 20), cv2.FONT_HERSHEY_SIMPLEX, 0.5, (0, 255, 0), 2)

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
