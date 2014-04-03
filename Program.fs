open System
open Trik
open Microsoft.FSharp.Core.Operators
open Microsoft.FSharp.Control

// red, gre, blu values are from 0 to 255
// hue = [0,360], saturation = [0,1], value = [0,1]
//		if s == 0, then h = -1 (undefined)
let RGBtoHSV red gre blu = 
    let red = float(red) / 255.0
    let gre = float(gre) / 255.0
    let blu = float(blu) / 255.0
    let min = min blu (min red gre)
    let max = max blu (max red gre)
    let delta = max - min
    let value = max
    // max = 0 => red = gre = blu = 0  
    // => saturation = 0, value is undefined
    if max = 0.0 then 
        let saturation = 0.0  
        let hue = -1
        (hue, saturation, value)
    elif red = max then
            let hue = (gre - blu) / delta
            let saturation = delta / max
            let mutable iHue = int(hue * 60.0)
            while iHue < 0 do iHue <- iHue + 360
            (iHue, saturation, value)
        elif gre = max then
            let hue = 2.0 + float(blu - red) / delta
            let saturation = delta / max
            let mutable iHue = int(hue * 60.0)
            while iHue < 0 do iHue <- iHue + 360
            (iHue, saturation, value)
       else
            let hue = 4.0 + float(red - gre) / delta
            let saturation = delta / max 
            let mutable iHue = int(hue * 60.0)
            while iHue < 0 do iHue <- iHue + 360
            (iHue, saturation, value)

// hue = [0,360], saturation = [0,1], value = [0,1]
// red, gre, blu values are from 0 to 255
let HSVtoRGB hue saturation value =
    // If Saturation is 0, all colors are the same
    // This is some flavor of gray
    if saturation = 0.0 then
        let red = int(value * 255.0)
        let gre = int(value * 255.0)
        let blu = int(value * 255.0)
        (red, gre, blu)
    else
        // The color wheel consists of 6 sectors
        // Figure out which sector you are in
        let sectorPos = float(hue) / 60.0
        let sectorNumber = hue / 60
        let fractionalSector = sectorPos - float(sectorNumber)
        // Calculate values for the three axis of the color
        let p = value * (1.0 - saturation)
        let q = value * (1.0 - (saturation * fractionalSector))
        let t = value * (1.0 - (saturation * (1.0 - fractionalSector)))
        // Assign the fractional colors to r, g, and b
        // based on the sector the angle is in
        match sectorNumber with
            |0 -> 
                let red = int(value * 255.0)
                let gre = int(t * 255.0)
                let blu = int(p * 255.0)
                (red, gre, blu)
            | 1 ->
                let red = int(q * 255.0)
                let gre = int(value * 255.0)
                let blu = int(p * 255.0)
                (red, gre, blu)
            | 2 ->
                let red = int(p * 255.0)
                let gre = int(value * 255.0)
                let blu = int(t * 255.0)
                (red, gre, blu)
            | 3 ->
                let red = int(p * 255.0)
                let gre = int(q * 255.0)
                let blu = int(value * 255.0)
                (red, gre, blu)
            | 4 ->
                let red = int(t * 255.0)
                let gre = int(p * 255.0)
                let blu = int(value * 255.0)
                (red, gre, blu)
            | _ ->                              
                let red = int(value * 255.0)
                let gre = int(p * 255.0)
                let blu = int(q * 255.0)
                (red, gre, blu)

let file = new IO.StreamReader("color_log")

let mutable red = 0
let mutable gre = 0
let mutable blu = 0
let mutable previousRed = 0
let mutable previousGre = 0
let mutable previousBlu = 0

let millisecondsDueTime = 100

let mutable line = ""

let mutable counter = -1

Helpers.I2C.init "/dev/i2c-2" 0x48 1

while true do
    counter <- counter + 1
    let line = file.ReadLine()
    // Read red, gre, blu values once per second
    if (counter % 30 = 0) then
        previousRed <- red
        previousGre <- gre
        previousBlu <- blu
        let mas = line.Split [|' '|]
        let RGBNumber = int mas.[1]
        red <- 0xFF &&& (RGBNumber >>> 16)
        gre <- 0xFF &&& (RGBNumber >>> 8)
        blu <- 0xFF &&& (RGBNumber)
        let previousHue, previousSaturation, previousValue = RGBtoHSV previousRed previousGre previousBlu
        let hue, saturation, value = RGBtoHSV red gre blu
        let deltaHue = float(hue - previousHue) / 10.0
        let deltaSaturation = (saturation - previousSaturation) / 10.0
        let deltaValue = (value - previousValue) / 10.0
        for l = 1 to 10 do
            // Delay 100 milliseconds
            Async.Sleep millisecondsDueTime
            |> Async.RunSynchronously
            let stripe = new Trik.LedStripe(0x14, 0x15, 0x16, 0x17)
            let r, g, b = HSVtoRGB (int(float(previousHue) + float(l) * deltaHue)) (previousSaturation + float(l) * deltaSaturation) (previousValue + float(l) * deltaValue)
            stripe.SetPower(r, g, b)