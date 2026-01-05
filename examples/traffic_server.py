from flask import Flask, send_file, jsonify, request
import json
import os

app = Flask(__name__)

@app.route('/')
def index():
    return send_file('/tmp/traffic_viz.html')

@app.route('/data')
def data():
    try:
        with open('/tmp/traffic_data.json', 'r') as f:
            return f.read()
    except FileNotFoundError:
        return jsonify({'error': 'Data not found'}), 404

@app.route('/get_params')
def get_params():
    try:
        with open('/tmp/traffic_params.json', 'r') as f:
            data = json.load(f)
            return jsonify(data)
    except (FileNotFoundError, json.JSONDecodeError):
        # Return default params if file doesn't exist or is invalid
        return jsonify({
            'max_speed': 2.0,
            'max_force': 0.1,
            'separation_radius': 25.0,
            'alignment_radius': 50.0,
            'cohesion_radius': 50.0,
            'rotation_amount': 0.0
        })

@app.route('/update_params', methods=['POST'])
def update_params():
    try:
        data = request.get_json()
        with open('/tmp/traffic_params.json', 'w') as f:
            json.dump(data, f)
        return jsonify({'status': 'ok'})
    except Exception as e:
        return jsonify({'status': 'error', 'message': str(e)}), 500

if __name__ == '__main__':
    app.run(host='0.0.0.0', port=8765, debug=False)
