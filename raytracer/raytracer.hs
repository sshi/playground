import System(getArgs)
import List(intersperse,sortBy)
import Maybe(isJust,isNothing,mapMaybe,listToMaybe,fromJust)

type V3 = (Float,Float,Float)
type Image = [[Pixel]]
type Pos = (Float,Float,Float)
data Obj = Sphere {position::Pos,sphere_radius::Float, color::Float} 
           deriving(Read,Show)

data RenderOption = RO {eye_pos::Pos,res::Float}
default_option = RO {eye_pos=(0,0,200),res=1}

type Pixel = Int
empty_pixel = 0

-- Utils

vadd (a,b,c) (d,e,f) = (a+d,b+e,c+f)
vdec (a,b,c) (d,e,f) = (a-d,b-e,c-f)
vpro (a,b,c) n = (a*n,b*n,c*n)
vsize (x,y,z) = sqrt (x*x + y*y + z*z)
unit_vector (x,y,z) = (x / r,y / r,z / r)
    where r = vsize (x,y,z)
inner_product (a,b,c) (d,e,f) = a*d+b*e+c*f

distance a b = vsize (vdec a b)
make_vec d s = unit_vector (vdec d s)

-- Main

main = do
  str <- getContents
  let (r,objectlist) = read str
      image = render objectlist (default_option {res=r})
  putP2 image

-- Define file sample
{-
(0.2, -- resolution
-- Object List
 [Sphere {position=(0, 0, -800),sphere_radius=100,color= 0.8}
 ,Sphere {position=(0, 50, -1000),sphere_radius=200,color= 0.7}
 ,Sphere {position=(0, 100, -1400),sphere_radius=300,color= 0.9}])
-}

--- generate ppm file

putP2 :: Image -> IO ()
putP2 image = do
  putStrLn "P2"
  putStrLn (show x ++ " " ++ show y)
  putStrLn "255"
  mapM_ putLine image
 where 
   putLine line = putStrLn $ concat $ intersperse " " (map show line) 
   y = length image
   x = length (head image)

--- make Image

render :: [Obj] -> RenderOption -> Image
render objlist rop = [[calc_pixel x y| x <- range] | y<-range]
    where
      eye = eye_pos rop
      r = res rop
      range = [-50,-50+r..50]
      calc_pixel x y = calc_ray objlist eye (make_vec (x,y,0) eye)

calc_ray :: [Obj] -> Pos -> V3 -> Pixel
calc_ray objlist eyepos eyevec 
    = case (first_hit objlist eyepos eyevec) of
        Nothing -> empty_pixel
        Just (obj,pos) -> shade obj pos eyevec

first_hit :: [Obj] -> Pos -> V3 -> Maybe (Obj,Pos)
first_hit objlist eyepos eyevec = listToMaybe sorted_list
    where hit_list = mapMaybe (calc_hit eyepos eyevec) objlist
          sorted_list = sortBy compare_obj hit_list
          compare_obj (_,p) (_,p') = compare 
                                     (distance eyepos p) 
                                     (distance eyepos p') 

calc_hit :: Pos -> V3 -> Obj -> Maybe (Obj,Pos)
calc_hit eye eyevec obj | isJust n = Just (obj,tpos)
                        | otherwise = Nothing
    where
      tpos = vadd eye (vpro eyevec (fromJust n))
      n = minroot 
          (inner_product eyevec eyevec)
          (2 * inner_product ptc eyevec)
          (inner_product ptc ptc - (sphere_radius obj ** 2))
              where
                ptc = vdec eye (position obj)

minroot :: Float -> Float -> Float -> Maybe Float
minroot a b c = if a == 0 then 
                    Just (-c / b)
                else 
                    if disc <= 0 then
                       Nothing
                    else
                        Just $ min 
                                 ((-b + discrt) / (2*a))
                                 ((-b - discrt) / (2*a))
    where
      disc = b ** 2 - 4 * a * c
      discrt = sqrt disc

shade :: Obj -> Pos -> V3 -> Pixel
shade obj hit_pos eyevec 
    = ceiling $ (lambert obj hit_pos eyevec) * color obj * 255
    where
      lambert obj@(Sphere {}) hit_pos eyevec 
          = max 0 (inner_product v' eyevec)
            where v' = make_vec (position obj) hit_pos
