// Web Audio API FFI bindings for PureScript

/**
 * Create a Web Audio AudioContext
 *
 * Note: Modern browsers require user interaction before allowing audio.
 * Call this in response to a button click or other user gesture.
 *
 * @returns {function} PureScript Effect that creates AudioContext
 */
export const createAudioContext = function() {
  return new (window.AudioContext || window.webkitAudioContext)();
};

/**
 * Schedule a single note to play
 *
 * Creates an oscillator connected to a gain node for volume control.
 * Schedules start and stop times relative to the audio context's current time.
 * Automatically cleans up the nodes after the note completes.
 *
 * @param {AudioContext} ctx - The Web Audio context
 * @param {Object} params - Note parameters
 * @param {number} params.time - Start time offset in seconds from now
 * @param {number} params.frequency - Frequency in Hz (e.g., 440 for A4)
 * @param {number} params.duration - Note duration in seconds
 * @param {number} params.volume - Amplitude 0.0 to 1.0
 * @param {string} params.waveform - Oscillator type: "sine", "square", "sawtooth", "triangle"
 * @returns {function} PureScript Effect Unit
 */
export const scheduleNote = function(ctx) {
  return function(params) {
    return function() {
      const now = ctx.currentTime;
      const startTime = now + params.time;
      const stopTime = startTime + params.duration;

      // Create oscillator (tone generator)
      const oscillator = ctx.createOscillator();
      oscillator.type = params.waveform;
      oscillator.frequency.setValueAtTime(params.frequency, startTime);

      // Create gain node (volume control)
      const gainNode = ctx.createGain();
      gainNode.gain.setValueAtTime(params.volume, startTime);

      // Apply basic envelope (fade out to avoid clicks)
      const fadeOutStart = stopTime - 0.01; // Last 10ms
      gainNode.gain.setValueAtTime(params.volume, fadeOutStart);
      gainNode.gain.linearRampToValueAtTime(0.0, stopTime);

      // Connect the audio graph: oscillator -> gain -> speakers
      oscillator.connect(gainNode);
      gainNode.connect(ctx.destination);

      // Schedule start and stop
      oscillator.start(startTime);
      oscillator.stop(stopTime);

      // Clean up nodes after they're done
      // (prevents memory leaks from accumulating nodes)
      oscillator.onended = function() {
        oscillator.disconnect();
        gainNode.disconnect();
      };
    };
  };
};
