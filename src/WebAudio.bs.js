// Generated by BUCKLESCRIPT VERSION 3.1.5, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");

var audioContext = new AudioContext();

var loadSound = (
  function (url, callback) {
    fetch(url)
      .then(response => response.arrayBuffer())
      .then(arrayBuffer => audioContext.decodeAudioData(arrayBuffer))
      .then(callback);
  }
);

var playBuffer = (
  function (buffer, note, gain, time, offsetRatio, durationRatio) {
    var playbackRate = Math.pow(2, note / 12);
    var offset = buffer.duration * offsetRatio;
    var duration = (buffer.duration - offset) * durationRatio;

    var gainNode = audioContext.createGain();
    gainNode.gain.value = gain;
    gainNode.gain.setValueAtTime(0, audioContext.currentTime);
    gainNode.gain.setTargetAtTime(1, time, 0.0005);
    gainNode.gain.setTargetAtTime(0, time + duration, 0.0005);

    var bufferSource = audioContext.createBufferSource();
    bufferSource.buffer = buffer;
    bufferSource.playbackRate.value = playbackRate;

    bufferSource.connect(gainNode);
    gainNode.connect(audioContext.destination);

    bufferSource.start(time, offset);
  }
);

function createSchedule(callback) {
  var beatTime = [0];
  var timeoutId = [/* None */0];
  var beatLength = 60 / 120 / 1;
  var onTimeout = function () {
    var targetTime = audioContext.currentTime + 0.2;
    while(beatTime[0] < targetTime) {
      Curry._2(callback, beatTime[0], beatLength);
      beatTime[0] += beatLength;
    };
    timeoutId[0] = /* Some */[setTimeout(onTimeout, 100)];
    return /* () */0;
  };
  var start = function () {
    var match = timeoutId[0];
    if (match) {
      clearTimeout(match[0]);
    }
    beatTime[0] = audioContext.currentTime;
    return onTimeout(/* () */0);
  };
  var stop = function () {
    var match = timeoutId[0];
    if (match) {
      clearTimeout(match[0]);
      return /* () */0;
    } else {
      return /* () */0;
    }
  };
  return /* record */[
          /* start */start,
          /* stop */stop
        ];
}

exports.audioContext = audioContext;
exports.loadSound = loadSound;
exports.playBuffer = playBuffer;
exports.createSchedule = createSchedule;
/* audioContext Not a pure module */
