namespace InstrumentControlDemo

open System
open System.Reactive.Linq
open FSharp.Control.Reactive
open FSharp.Data.UnitSystems.SI.UnitSymbols
open NAudio.Wave

[<AutoOpen>]
module Async =
     type Agent<'T> = MailboxProcessor<'T>

[<AutoOpen>]
module Audio =
    type WaveInEventArgs with
        /// Converts the byte array of samples to float32.
        member bytes.AsFloat32Array () = 
            Array.init (bytes.BytesRecorded / 4) 
            <| (fun n -> (BitConverter.ToSingle(bytes.Buffer, n * 4)))

    type WaveBuffer with
        /// Writes the given sequence of float32 samples to the byte array, starting
        /// at the specified index.
        static member WriteFloat32Seq (buffer : byte array, offset, samples) =
            let wavebuffer = new WaveBuffer(buffer)
            samples |> Seq.iteri (fun i x -> wavebuffer.FloatBuffer.[offset / 4 + i] <- x)

/// Time-domain functions.
module Function =
    let private pi = Math.PI

    /// Sine wave.
    let sine amplitude frequency (time : float<s>) =
        amplitude * (float32 <| sin (2.0 * pi * time * frequency))

    /// Cosine wave.
    let cosine amplitude frequency (time : float<s>) =
        amplitude * (float32 <| cos (2.0 * pi * time * frequency))

    /// Frequency sweep.
    let chirp amplitude initialFrequency chirpRate duration (time : float<s>) =
        let reps = floor (time / duration)
        let time' = time - reps * duration
        amplitude * (float32 <| sin (2.0 * pi * (initialFrequency * time'  + chirpRate * time' * time' / 2.0)))

    /// Discretely samples the nth sample of the given function with the given sample interval.
    let sample (sampleInterval : float<s>) f n : float32 =
        f (sampleInterval * (float n))

/// Observable DSP functions.
module Observable =
    
    /// Filters the observable elements of a sequence based on a predicate by 
    /// incorporating the element's index.
    ///
    /// Temporary fix for bug in FSharp.Control.Reactive 3.2.0 where Observable.filteri 
    /// is actually equivalent to Observable.filter.
    let filteri predicate (source: IObservable<'T>)  = 
        Observable.Where(source, Func<_,_,_> (fun x i -> predicate i x))

    /// Branches a single channel signal into a tuple of two identical signals.
    let tee source = source |> Observable.map (fun (x : float32) -> (x, x))

    /// Decimates a signal by the specified factor, only firing on every n-th observation.
    let decimate n source = source |> filteri (fun i _ -> i % n = 0)

    /// Computes the vector magnitude of a two-channel signal.
    let magnitude source = source |> Observable.map (fun (x, y) -> sqrt (x * x + y * y) : float32)

    /// Mixes a single channel signal with a function by multiplying the i-th observation with the
    /// value of the given function at i.
    let mix f source = source |> Observable.mapi (fun i x -> x * (f () i) : float32)

    /// Mixes a two-channel signal with a pair of functions by multiplying (element-wise) the i-th
    /// observation with the value of the given functions at i.
    let mixPair fx fy source =
        source |> Observable.mapi (fun i (x, y) -> (x * (fx () i), y * (fy () i)) : float32 * float32)
    
    /// Applies a single-pole infinite impulse response low-pass filter with the given filter
    /// coefficient to the signal.
    let lowPass coeff source =
        source |> Observable.scanInit 0.0f (fun out1 in0 -> 
            let x = coeff ()
            in0 * (1.0f - x) + out1 * x)

    /// Applies a single-pole infinite impulse response low-pass filter with the given filter
    /// coefficient to each channel of a two-channel signal.
    let lowPassPair coeff source =
        source |> Observable.scanInit (0.0f, 0.0f) (fun (out1x, out1y) (in0x, in0y) -> 
            let x = coeff ()
            (in0x * (1.0f - x) + out1x * x, in0y * (1.0f - x) + out1y * x))