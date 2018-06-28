type audioContext;
type buffer;

[@bs.new] external create_audioContext : unit => audioContext = "AudioContext";
[@bs.get] external getCurrentTime : (audioContext) => float = "currentTime";

let audioContext = create_audioContext();

let loadSound: (string, (buffer) => unit) => unit = [%bs.raw {|
  function (url, callback) {
    fetch(url)
      .then(response => response.arrayBuffer())
      .then(arrayBuffer => audioContext.decodeAudioData(arrayBuffer))
      .then(callback);
  }
|}];

let playBuffer: (buffer, int, float, float, float, float) => unit = [%bs.raw {|
  function (buffer, note, gain, delay, offsetRatio, durationRatio) {
    var playbackRate = Math.pow(2, note / 12);
    var offset = buffer.duration * offsetRatio;
    var duration = (buffer.duration - offset) * durationRatio;

    var gainNode = audioContext.createGain();
    gainNode.gain.value = gain;

    var bufferSource = audioContext.createBufferSource();
    bufferSource.buffer = buffer;
    bufferSource.playbackRate.value = playbackRate;

    bufferSource.connect(gainNode);
    gainNode.connect(audioContext.destination);

    bufferSource.start(delay, offset, duration);
  }
|}];

type schedule = {
  start: (unit) => unit,
  stop: (unit) => unit
};

let createSchedule = (callback) => {
  let beatTime = ref(0.);
  let timeoutId = ref(None);
  let bpm = 120.;
  let ticksPerBeat = 1.;
  let beatLength = 60. /. bpm /. ticksPerBeat;

  let rec onTimeout = () => {
    let targetTime = getCurrentTime(audioContext) +. 0.2;

    while (beatTime^ < targetTime) {
      callback(beatTime^, beatLength);

      beatTime := beatTime^ +. beatLength;
    };

    timeoutId := Some(Js.Global.setTimeout(onTimeout, 100));
  };

  let start = () => {
    switch (timeoutId^) {
      | None => ()
      | Some(i) => Js.Global.clearTimeout(i)
    };

    beatTime := getCurrentTime(audioContext);

    onTimeout();
  };

  let stop = () => {
    switch (timeoutId^) {
      | None => ()
      | Some(i) => Js.Global.clearTimeout(i)
      };
  };

  {
    start,
    stop
  }
};
