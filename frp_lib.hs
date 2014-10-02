module Frp_lib where

type Signal a = [a]
type Time=Int
data Event a = NoEvent | Event a deriving (Show , Eq)
type SF a b = Signal a -> Signal b

--creates a constant singal
constant:: a -> Signal a
constant x = (repeat x)

--gives out the same signal without any change
identity :: Signal a -> Signal a
identity x= x

--creates a signal Event based on a boolean condition
edge:: SF Bool (Event())
edge [] = []
edge [b] = [NoEvent]
edge (b:bs) = (abc:(edge bs))
	where abc = if(b==False && head(bs) == True) then Event() else NoEvent

notyet:: SF (Event a) (Event a)
notyet (a:as) = (NoEvent:as)

--creates a Signal of NoEvent
never::Signal(Event a)
never = (repeat NoEvent)

--creates signal of Events at regular intervals
repeatedly :: Time -> b -> SF a (Event b)
repeatedly q b = \l -> [Event b] ++ (take q (repeat NoEvent))  ++ (repeatedly q b l)

-- holds on to a default value in case of noEvent and gives out the value inside the event in case of event
hold :: a -> SF (Event a) a
hold a (b:bs) = (holdAux b: hold a bs)
    where holdAux NoEvent = a
	  holdAux x = extfrm x
		where extfrm (Event x) = x

--Creates a Signal function that is capable of producing event after a specific period of time
after :: Time -> b -> SF a (Event b)
after q x (c:cs)= take q (repeat NoEvent) ++ [Event x] ++ (repeat NoEvent)


--wrapper function of integration
integrate :: Num a => SF a a
integrate a = integral 0 a

--Calculates integral using the rectangle rule
integral :: Num a => a -> SF a a
integral a (b:bs) = ((a+b) : integral (a+b) bs)
integral a [] = [a]

--Does integration
iIntegral :: Num a => a -> SF a a
iIntegral x a =  map (\y -> x + y) (integrate a)

--creates an event immediately
now:: b -> SF a (Event b)
now b _ = (Event b : repeat(NoEvent))

--creates an event in the next cycle
delay::Time -> a -> SF a a
delay t ax a= (take t (repeat ax)) ++ a

arr :: (a -> b) -> SF a b
arr x = \ y -> map x y

--makes an Event
tag :: Event a -> b -> Event b
tag (Event a) y = Event y
tag NoEvent y =NoEvent

-- uses a event producing signal function to switch signal functions
switchWhen :: Eq e => Signal a -> SF a (Event e) -> (e -> Signal a) -> Signal a
switchWhen sf sfe = switch ((identity &&& sfe) sf)

-- switches between one signal function and another
switch :: Eq c => Signal ( a , Event c) -> ( c -> Signal a) -> Signal a
switch x y = switch_aux x y NoEvent

-- auxillary function used by switch
switch_aux :: Eq c => Signal (a,Event c) -> (c -> Signal a) -> Event c -> Signal a
switch_aux ((x1,x2):xs) y NoEvent = (x1: if x2 == NoEvent then switch_aux xs y NoEvent else switch_aux xs y x2)
switch_aux xs y (Event c) = y c

(&&&) :: (t -> Signal a) -> (t -> Signal b) -> (t -> Signal (a,b))
(&&&) a b = \x -> zip (a x) (b x)

------- Animation code using the Library above----------

type Acceleration = Double
type Velocity     = Double
type Position     = Double
type Radius 	  = Double

--Defining a ball type
data Ball         = Ball { ballRad  :: Radius
                         , ballPosX :: Position
                         , ballPosY :: Position
                         , ballVel  :: Velocity
                         } deriving Eq
g :: Acceleration
g = -1

ball1 :: Ball
ball1 = Ball { ballRad  = 0.04
             , ballPosX = 0.5
             , ballPosY = 0.8
             , ballVel  = - 0.0025
             }

ball_tagger :: Signal Ball -> Signal (Event a) -> Signal (Event Ball)
ball_tagger (b:bs) (e:es) = (tag e (b{ballVel= ballVel b * (-1)}) : ball_tagger bs es)

-- defining a bounce detector that will produce an event when the ball hits the ground
detectBounce :: SF Ball (Event Ball)
detectBounce balls =  ball_tagger balls (edge (map (\ x -> (ballPosY x <= ballRad x) || (ballPosY x >=0.8)) balls))

--this is the main singal function which is run recursively			 
bouncingBall :: Ball -> Signal Ball
bouncingBall b = switchWhen (moving b) detectBounce bouncingBall

--This function produces the signal of Ball at it consecutive position by using integrating the velocity to
moving:: Ball -> Signal Ball
moving b = map (\x -> b {ballPosY = x}) (iIntegral (ballPosY b) (repeat (ballVel b)))



 
