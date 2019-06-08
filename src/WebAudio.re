type audioContext;
type globalFx;

[@bs.new] external create_audioContext : unit => audioContext = "AudioContext";
[@bs.get] external getCurrentTime : (audioContext) => float = "currentTime";

let audioContext = create_audioContext();

let resume: unit => unit = [%bs.raw {|
  function () {
    audioContext.resume();
  }
|}];

let createGlobalFx: unit => globalFx = [%bs.raw {|
  function (_) {
    function createConvolver () {
      const seconds = 2;
      const decay = 2; // to 100?
      const length = Math.floor(seconds * audioContext.sampleRate);
      const impulse = audioContext.createBuffer(2, length, audioContext.sampleRate);
      const impulseL = impulse.getChannelData(0);
      const impulseR = impulse.getChannelData(1);

      /*
      for (let i = 0; i < length; i++) {
        impulseL[i] = impulseR[i] = -1 + (2 * Math.random());
      }
      */

      for (let i = 0; i < length; i++) {
        impulseL[i] = (Math.random() * 2 - 1) * Math.pow(1 - i / length, decay);
        impulseR[i] = (Math.random() * 2 - 1) * Math.pow(1 - i / length, decay);
      }

      const convolver = audioContext.createConvolver();
      convolver.buffer = impulse;

      return convolver;
    }

    const globalFx = {
      masterGain: audioContext.createGain(),
      convolver: createConvolver(),
      convolverGain: audioContext.createGain(),
      warbleOsc: audioContext.createOscillator(),
      warbleGain: audioContext.createGain()
    };

    globalFx.masterGain.connect(audioContext.destination);

    globalFx.convolver.connect(globalFx.convolverGain);
    globalFx.convolverGain.connect(globalFx.masterGain);

    globalFx.convolverGain.gain.value = 0.8;

    globalFx.warbleOsc.frequency.value = 2;
    globalFx.warbleOsc.connect(globalFx.warbleGain);
    globalFx.warbleOsc.start();

    globalFx.warbleGain.gain.value = 2.1;

    return globalFx;
  }
|}];

let setGlobalVolume: float => unit = [%bs.raw {|
  function (volume) {
    globalFx.masterGain.gain.value = Math.pow(volume, 1.6);
  }
|}];

let globalFx = createGlobalFx();

let fundamental = 40;
let ratios = [|2., 3., 4.16, 5.43, 6.79, 8.21|];

let playHihat: (~start:float) => unit = [%bs.raw {|
  function (start) {
    const gain = audioContext.createGain();
    const random = Math.random();

    // Bandpass
    const bandpass = audioContext.createBiquadFilter();
    bandpass.type = "bandpass";
    bandpass.frequency.value = 10000 * (1 + random);

    // Highpass
    const highpass = audioContext.createBiquadFilter();
    highpass.type = "highpass";
    highpass.frequency.value = 7000 * (1 + (random * 0.3));

    // Connect the graph
    bandpass.connect(highpass);
    highpass.connect(gain);
    gain.connect(globalFx.masterGain);

    // Create the oscillators
    ratios.forEach(function(ratio) {
      const osc = audioContext.createOscillator();
      osc.type = "square";
      // Frequency is the fundamental * this oscillator's ratio
      osc.frequency.value = fundamental * ratio;
      osc.connect(bandpass);
      osc.start(start);
      osc.stop(start + 0.3);
    });

    // Define the volume envelope
    gain.gain.setValueAtTime(0.00001, start);
    gain.gain.exponentialRampToValueAtTime(1 - (random * 0.15), start + 0.02);
    gain.gain.exponentialRampToValueAtTime(0.3 - (random * 0.15), start + 0.03);
    gain.gain.exponentialRampToValueAtTime(0.00001, start + 0.3);
  }
|}];

let playOsc: unit => unit = [%bs.raw {|
  function (note, start, time, gain, output) {
    const frequency = 440 * Math.pow(2, note / 12);

    // create nodes
    const osc = audioContext.createOscillator();
    osc.type = 'square';
    osc.frequency.value = frequency;

    const gainNode = audioContext.createGain();
    gainNode.gain.value = gain;

    osc.start(start);
    osc.stop(start + time);

    // routing
    osc.connect(gainNode);
    gainNode.connect(output);

    globalFx.warbleGain.connect(osc.frequency);
  }
|}];

let synthFilterMin = 100.0;
let synthFilterMax = 22000.0;
let synthFilterRange = synthFilterMax -. synthFilterMin;
let synthFilterLog = log(synthFilterMax /. synthFilterMin) /. log(2.0);

let playSynth: (~note:int, ~chord:array(int), ~gain:float, ~pan:float, ~start:float, ~time:float, ~filter:float) => unit = [%bs.raw {|
  function (note, chords, gain, pan, start, time, filter) {
    const filterLogScale = synthFilterMin + (synthFilterRange * Math.pow(2, synthFilterLog * (filter - 1)));

    const lowpass = audioContext.createBiquadFilter();
    lowpass.type = 'lowpass';
    lowpass.frequency.value = filterLogScale;

    gain = Math.pow(gain, 1.6);

    const gainNode = audioContext.createGain();
    gainNode.gain.value = gain;

    const stereoPannerNode = audioContext.createStereoPanner();
    stereoPannerNode.pan.value = pan;

    // schedule
    gainNode.gain.setValueAtTime(gain, start);
    gainNode.gain.setTargetAtTime(0, start, Math.max(0.05, time - 0.05));

    gainNode.connect(lowpass);

    lowpass.connect(stereoPannerNode);

    stereoPannerNode.connect(globalFx.masterGain);
    stereoPannerNode.connect(globalFx.convolver);

    const voiceGain = chords.length === 0 ? 1 : 1 / chords.length;

    playOsc(note, start, time, voiceGain, gainNode);

    chords.forEach(function (noteOffset) {
      playOsc(note + noteOffset, start, time, voiceGain, gainNode);
    });
  }
|}];

type schedule = {
  start: (unit) => unit,
  stop: (unit) => unit,
  setBpm: (float) => unit
};

let createSchedule = (callback) => {
  let beatTime = ref(0.);
  let timeoutId = ref(None);
  let bpm = ref(120.);
  let ticksPerBeat = 4.;

  let rec onTimeout = () => {
    let targetTime = getCurrentTime(audioContext) +. 0.2;
    let beatLength = 60. /. bpm^ /. ticksPerBeat;

    // "stop" may be called during the callback, so the timeout must be set
    // first.
    timeoutId := Some(Js.Global.setTimeout(onTimeout, 100));

    while (beatTime^ < targetTime) {
      callback(beatTime^, beatLength);

      beatTime := beatTime^ +. beatLength;
    };
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

  let setBpm = (value) => {
    bpm := value;
  };

  {
    start,
    stop,
    setBpm
  };
};
