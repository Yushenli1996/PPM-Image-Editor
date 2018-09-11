//
// F#-based PPM image library.
//
// <<Yushen Li>>
// U. of Illinois, Chicago
// CS341, Spring 2018
// Project 06
//

module PPMImageLibrary

open System.Security.Cryptography
open System.Net
open System

#light


//
// DebugOutput:
//
// Outputs to console, which appears in the "Output" window pane of
// Visual Studio when you run with debugging (F5).
//
let rec private OutputImage (image:(int*int*int) list list) = 
  match image with
  | [] -> printfn "**END**"
  | hd::tl -> printfn "%A" hd
              OutputImage tl
           
let DebugOutput(width:int, height:int, depth:int, image:(int*int*int) list list) =
  printfn "**HEADER**"
  printfn "W=%A, H=%A, D=%A" width height depth
  printfn "**IMAGE**"
  OutputImage image


//
// TransformFirstThreeRows:
//
// An example transformation: replaces the first 3 rows of the given image
// with a row of Red, White and Blue pixels (go USA :-).
//
let rec BuildRowOfThisColor row color = 
  match row with
  | []     -> []
  | hd::tl -> color :: BuildRowOfThisColor tl color

let TransformFirstThreeRows(width:int, height:int, depth:int, image:(int*int*int) list list) = 
  let row1 = List.head image
  let row2 = List.head (List.tail image)
  let row3 = List.head (List.tail (List.tail image))
  let tail = List.tail (List.tail (List.tail image))
  let newRow1 = BuildRowOfThisColor row1 (255,0,0)      // red:
  let newRow2 = BuildRowOfThisColor row2 (255,255,255)  // white:
  let newRow3 = BuildRowOfThisColor row3 (0,0,255)      // blue:
  let newImage = newRow1 :: newRow2 :: newRow3 :: tail
  newImage


//
// WriteP3Image:
//
// Writes the given image out to a text file, in "P3" format.  Returns true if successful,
// false if not.
//
let Flatten (SL:string list) = 
  List.reduce (fun s1 s2 -> s1 + " " + s2) SL

let Image2ListOfStrings (image:(int*int*int) list list) = 
  List.map (fun TL -> List.map (fun (r,g,b) -> r.ToString()+" "+g.ToString()+" "+b.ToString()+" ") TL) image
  |> List.map Flatten

let rec WriteP3Image(filepath:string, width:int, height:int, depth:int, image:(int*int*int) list list) = 
  let L = [ "P3" ] @ 
          [ System.Convert.ToString(width); System.Convert.ToString(height) ] @
          [ System.Convert.ToString(depth) ] @
          (Image2ListOfStrings image)
  System.IO.File.WriteAllLines(filepath, L)
  true  // success



//
// Grayscale:
//
// Converts the image into grayscale and returns the resulting image as a list of lists. 
// Conversion to grayscale is done by averaging the RGB values for a pixel, and then 
// replacing them all by that average. So if the RGB values were 25 75 250, the average 
// would be 116, and then all three RGB values would become 116 — i.e. 116 116 116.
//
let rec Grayscale(width:int, height:int, depth:int, image:(int*int*int) list list) = 
  List.map(fun list -> (List.map(fun (r,g,b) -> if((r+g+b)/3) = 0 then (0,0,0)
                                                else (((r+g+b)/3),((r+g+b)/3),((r+g+b)/3))) list)) image



//
// Threshold
//
// Thresholding increases image separation --- dark values become darker and light values
// become lighter.  Given a threshold value in the range 0 < threshold < MaxColorDepth,
// all RGB values > threshold become the max color depth (white) while all RGB values
// <= threshold become 0 (black).  The resulting image is returned.
//
let rec Threshold(width:int, height:int, depth:int, image:(int*int*int) list list, threshold:int) = 
  List.map(fun list -> (List.map(fun(r,g,b) -> if(r > threshold && g > threshold && b > threshold) then (255, 255, 255)
                                               elif(r <= threshold && g > threshold && b > threshold) then (0, 255, 255)
                                               elif(r <= threshold && g <= threshold && b > threshold) then (0, 0, 255)
                                               elif(r <= threshold && g <= threshold && b <= threshold) then (0, 0, 0)
                                               elif(r > threshold && g <= threshold && b > threshold) then (255, 0, 255)
                                               elif(r > threshold && g <= threshold && b <= threshold) then (255, 0, 0)
                                               else (255,255,0))) list) image



//
// FlipHorizontal:
//
// Flips an image so that what’s on the left is now on the right, and what’s on 
// the right is now on the left. That is, the pixel that is on the far left end
// of the row ends up on the far right of the row, and the pixel on the far right 
// ends up on the far left. This is repeated as you move inwards toward the center 
// of the row.
//
let rec FlipHorizontal(width:int, height:int, depth:int, image:(int*int*int) list list) = 
  List.map(fun list -> List.rev list) image



//
// Zoom:
//
// Zooms the image by the given zoom factor, which is an integer 0 < factor < 5.  
// The function uses the nearest neighbor approach where each pixel P in the original 
// image is replaced by a factor*factor block of P pixels.  For example, if the zoom 
// factor is 4, then each pixel is replaced by a 4x4 block of 16 identical pixels. 
// The nearest neighbor algorithm is the simplest zoom algorithm, but results in 
// jagged images.  The resulting image is returned.
//
let Zoompixel pixel factor = 
    let L = [1..factor]
    List.map(fun i -> pixel) L
    
    
let expandRow row factor = 
    let A = List.map(fun pixel -> Zoompixel pixel factor) row
    let result = List.reduce(fun acc tuples -> tuples @ acc) A
    List.rev result
    
let expandCol row factor =
    let L = [1..factor]
    List.map(fun i -> expandRow row factor) L
    

let rec Zoom(width:int, height:int, depth:int, image:(int*int*int) list list, factor:int) = 
    let L = List.map(fun row -> expandCol row factor) image
    List.reduce(fun acc list -> acc @ list) L


//
// RotateRight90:
//
// Rotates the image to the right 90 degrees.
//

let rec _tupleToList row accumulator = 
    match row with
    |[]-> List.rev accumulator
    |a::[] -> List.rev ([a]::accumulator)
    |hd::tl -> _tupleToList tl ([hd] :: accumulator)

let tupleToList row = 
    _tupleToList row []

let rec _mergeList list accumulator = 
    match list with
    |[]-> List.rev accumulator
    |a::[] -> List.rev (a::accumulator)
    |hd1::hd2::tl -> _mergeList tl ((List.map2(fun x y -> x @ y ) hd1 hd2)::accumulator)

let rec mergeList list = 
    if((List.length list) < 2) then list
    else mergeList (_mergeList list [])
    
let rec RotateRight90(width:int, height:int, depth:int, image:(int*int*int) list list) = 
    let L = List.map(fun list -> tupleToList list) image
    let product = mergeList L
    let result = List.reduce(fun acc elem -> acc @ elem) product
    List.map(fun row -> List.rev row) result
    //result
  
