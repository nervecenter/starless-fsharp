// Math module
//import numpy as np
// Plotting module
//import matplotlib.pyplot as plt
// Plotting image module (unused?)
//import matplotlib.image as mpimg
// Image processing tools module
//import scipy.ndimage as ndim
// imread, imsave, etc
//import scipy.misc as spm
//import random,sys,time,os
//import datetime

open System.IO

(********************************  
 *   STRUCTURES AND CONSTANTS   *
 ********************************)

// enums
let METH_LEAPFROG =         0
let METH_RK4 =              1

// rough option parsing
type Config_r = {
    LOFI :                  bool;
    DISABLE_DISPLAY :       bool;
    DISABLE_SHUFFLING :     bool;
    NTHREADS :              int;
    DRAWGRAPH :             bool;
    OVERRIDE_RES :          bool;
    SCENE_FNAME :           string;
    CHUNKSIZE :             int;
    RESOLUTION :            int * int;
}

type Vector3 = {
    x :     double;
    y :     double;
    z :     double;
}

type RenderConfig_r = {
    Distort :               int;
    Fogdo :                 int;
    Blurdo :                int;
    Fogmult :               double;
    Diskinner :             double;
    Diskouter :             double;
    Resolution :            int * int;
    Diskmultiplier :        double;
    Gain :                  int;
    Normalize :             int;
    Bloomcut :              double;
    Airy_bloom :            int;
    Airy_radius :           double;
    Iterations :            int;
    Stepsize :              double;
    Cameraposition :        Vector3;
    Fieldofview :           double;
    Lookat :                Vector3;
    Horizongrid :           int;
    Redshift :              int;
    sRGBOut :               int;
    Diskintensitydo :       int;
    sRGBIn :                int;
}

(*****************  
 *   FUNCTIONS   *
 *****************)

// readargs, for configuring our config from our arguments
let readargs argv =
    let argl = List.ofArray argv
    let defaultConfig = {
        LOFI =                  false;
        DISABLE_DISPLAY =       false;
        DISABLE_SHUFFLING =     false;
        NTHREADS =              4;
        DRAWGRAPH =             true;
        OVERRIDE_RES =          false;
        SCENE_FNAME =           "scenes/default.scene";
        CHUNKSIZE =             9000;
        RESOLUTION =            (800,600);
    }

    let rec changeConfig (args:List<string>) config =
        if args.IsEmpty then config else
        let newConfig = match args.Item(0) with
                        | "-d" -> { config with LOFI = true }
                        | "--no-graph" -> { config with DRAWGRAPH = false } 
                        | "--no-display" -> { config with DISABLE_DISPLAY = true }
                        | "--no-shuffle" -> { config with DISABLE_SHUFFLING = true }
                        | "-o" | "--no-bs" -> { config with DRAWGRAPH = false; DISABLE_DISPLAY = true; DISABLE_SHUFFLING = true }
                        | arg when arg.Substring(0,2) = "-c" -> { config with CHUNKSIZE = int(arg.Substring(2)) }
                        | arg when arg.Substring(0,2) = "-j" -> { config with NTHREADS = int(arg.Substring(2)) }
                        | arg when arg.Substring(0,2) = "-r" -> let res = arg.Substring(2).Split('x')
                                                                let xy = ( int(res.[0]), int(res.[1]) )
                                                                { config with RESOLUTION = xy; OVERRIDE_RES = true }
                        | arg when arg.Chars(0) = '-' -> printfn "Unrecognized option: %s" arg
                                                         exit 1
                        | arg -> { config with SCENE_FNAME = arg }
                        | _ -> config
        changeConfig (List.tail args) newConfig
    changeConfig argl defaultConfig
// End readargs

let checkSceneFile filename =
    match File.Exists(filename) with
    | true -> ()
    | false -> printfn "Scene file \"%s\" does not exist." filename
               exit 2

// readSceneFile, for fetching render configuration
let readSceneFile filename =
    printfn "Reading scene \"%s\"..." filename
    use s = new StreamReader(filename)
    let defaultRConfig = {
        Distort =               1;
        Fogdo =                 1;
        Blurdo =                1;
        Fogmult =               0.02;
        Diskinner =             1.5;
        Diskouter =             4.0;
        Resolution =            (160, 120);
        Diskmultiplier =        100.0;
        Gain =                  1;
        Normalize =             -1;
        Bloomcut =              2.0;
        Airy_bloom =            1;
        Airy_radius =           1.0;
        Iterations =            1000;
        Stepsize =              0.02;
        Cameraposition =        { x = 0.0; y = 1.0; z = -10.0 };
        Fieldofview =           1.5;
        Lookat =                { x = 0.0; y = 0.0; z = 0.0 };
        Horizongrid =           1;
        Redshift =              1;
        sRGBOut =               1;
        Diskintensitydo =       1;
        sRGBIn =                1;
    }

    // THERE ARE HIFI AND LOFI VERSIONS OF RESOLUTION, ITERATIONS, STEPSIZE
    // NEED TO ADD UPVECTOR FOR GEOMETRY SECTION
    // NEED TO ASK TEXTURES IN MATERIALS
    // ALSO SKYDISKRATIO IN MATERIALS

    let rec readRenderConfig renderConfig (sr:StreamReader) =
        if sr.Peek() = -1 then renderConfig else
        let line = sr.ReadLine()
        let newRenderConfig = match line.Split('=') with
                              | line when line.[0] = "Distort" -> { renderConfig with Distort = int(line.[1]) }
                              | line when line.[0] = "Fogdo" -> { renderConfig with Fogdo = int(line.[1]) }
                              | line when line.[0] = "Blurdo" -> { renderConfig with Blurdo = int(line.[1]) }
                              | line when line.[0] = "Fogmult" -> { renderConfig with Fogmult = double(line.[1]) }
                              | line when line.[0] = "Diskinner" -> { renderConfig with Diskinner = double(line.[1]) }
                              | line when line.[0] = "Diskouter" -> { renderConfig with Diskouter = double(line.[1]) }
                              | line when line.[0] = "Resolution" -> { renderConfig with Resolution = let xy = line.[1].Split(',')
                                                                                                      ( int(xy.[0]), int(xy.[1]) ) }
                              | line when line.[0] = "Diskmultiplier" -> { renderConfig with Diskmultiplier = double(line.[1]) }
                              | line when line.[0] = "Gain" -> { renderConfig with Gain = int(line.[1]) }
                              | line when line.[0] = "Normalize" -> { renderConfig with Normalize = int(line.[1]) }
                              | line when line.[0] = "Bloomcut" -> { renderConfig with Bloomcut = double(line.[1]) }
                              | line when line.[0] = "Airy_bloom" -> { renderConfig with Airy_bloom = int(line.[1]) }
                              | line when line.[0] = "Airy_radius" -> { renderConfig with Airy_radius = double(line.[1]) }
                              | line when line.[0] = "Iterations" -> { renderConfig with Iterations = int(line.[1]) }
                              | line when line.[0] = "Stepsize" -> { renderConfig with Stepsize = double(line.[1]) }
                              | line when line.[0] = "Cameraposition" -> { renderConfig with Cameraposition = let coords = line.[1].Split(',')
                                                                                                              { x = double(coords.[0]); y = double(coords.[1]); z = double(coords.[2]) } }
                              | line when line.[0] = "Fieldofview" -> { renderConfig with Fieldofview = double(line.[1]) }
                              | line when line.[0] = "Lookat" -> { renderConfig with Lookat = let coords = line.[1].Split(',')
                                                                                              { x = double(coords.[0]); y = double(coords.[1]); z = double(coords.[2]) } }
                              | line when line.[0] = "Horizongrid" -> { renderConfig with Horizongrid = int(line.[1]) }
                              | line when line.[0] = "Redshift" -> { renderConfig with Redshift = int(line.[1]) }
                              | line when line.[0] = "sRGBOut" -> { renderConfig with sRGBOut = int(line.[1]) }
                              | line when line.[0] = "Diskintensitydo" -> { renderConfig with Diskintensitydo = int(line.[1]) }
                              | line when line.[0] = "sRGBIn" -> { renderConfig with sRGBIn = int(line.[1]) }
                              | _ -> renderConfig
        readRenderConfig newRenderConfig sr
    readRenderConfig defaultRConfig s
// End readSceneFile

(********************  
 *   MAIN FUNCTION  *
 ********************)

[<EntryPoint>]
let main argv = 
    let argv = [|"scenes/rings.scene"|]
    let config = readargs argv
    checkSceneFile config.SCENE_FNAME
    let renderConfig = readSceneFile config.SCENE_FNAME
    0
