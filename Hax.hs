module Hax where

data UFunction = Cos | Sin | Tan | Cot | Exp | Minus | Sigmoid | Identity deriving (Show)
data BFunction = Add | Mul | Sub | Div deriving (Show)
-- data Loop = FOR
-- add parameter to graph to input and return type like bool, double, int, or matrix
data Graph=NodeB BFunction Graph Graph | NodeU UFunction Graph | NodeT UFunction | NodeC Double| If Graph Graph Graph deriving (Show)

sigmoid:: Double -> Double
sigmoid x= 1.0/(1+exp(-x))
dsigmoid x=let s=sigmoid(x)
           in s*(1-s)


gradient :: Graph -> Graph
gradient (NodeB f g h) = case f of
                           Add -> NodeB Add (gradient g) (gradient h)
                           Mul -> NodeB Add (NodeB Mul (gradient g) h) (NodeB Mul g (gradient h))
                           Sub -> NodeB Sub (gradient g) (gradient h)
                           Div -> NodeB Div (NodeB Sub (NodeB Mul (gradient g) h) (NodeB Mul g (gradient h))) (NodeB Mul h h)
gradient (NodeU f g)   = case f of
                           Sin -> NodeB Mul (gradient g) (NodeU Cos g)
                           Cos -> NodeU Minus (NodeB Mul (gradient g) (NodeU Sin g))
                           Exp -> NodeB Mul (gradient g) (NodeU Exp g)
                           Sigmoid -> NodeB Mul (gradient g) (NodeB Mul (NodeU Sigmoid g) (NodeB Sub (NodeC 1.0) (NodeU Sigmoid g)) )
                           Minus -> NodeU Minus (gradient g)
                           Identity -> gradient g
gradient (NodeT f)     = case f of
                           Sin -> NodeT Cos
                           Cos -> NodeU Minus (NodeT Sin)
                           Exp -> NodeT Exp
                           Sigmoid -> NodeB Mul (NodeT Sigmoid) (NodeB Sub (NodeC 1.0) (NodeT Sigmoid)) 
                           Minus -> NodeT Minus
                           Identity -> NodeC 1.0
gradient (NodeC _ )    = NodeC 0.0
gradient (If c h g)    = If c (gradient h) (gradient g)


                     

evaluate :: Graph -> Double -> Double
evaluate (NodeB f g h) x = case f of
                             Add -> (evaluate g x) + (evaluate h x)
                             Mul -> (evaluate g x) * (evaluate h x)
                             Sub -> (evaluate g x) - (evaluate h x)
                             Div -> (evaluate g x) / (evaluate h x)
evaluate (NodeU f g) x   = case f of
                             Sin -> sin $ evaluate g x
                             Cos -> cos $ evaluate g x
                             Exp -> exp $ evaluate g x
                             Sigmoid -> sigmoid $ evaluate g x
                             Minus -> -(evaluate g x)
                             Identity -> evaluate g x
evaluate (NodeT f) x     = case f of
                             Sin -> sin x
                             Cos -> cos x
                             Exp -> exp x
                             Sigmoid -> sigmoid x
                             Minus -> -x
                             Identity -> x
evaluate (NodeC y) _     = y
evaluate (If c g h) x    = if (evaluate c x) > 0.0
                             then evaluate g x
                             else evaluate h x
                     
tofunction :: Graph -> ( Double -> Double)
tofunction (NodeB f g h) = case f of
                             Add -> (\x -> (tofunction g x) + (tofunction h x))
                             Mul -> (\x -> (tofunction g x) * (tofunction h x))
                             Sub -> (\x -> (tofunction g x) - (tofunction h x))
                             Div -> (\x -> (tofunction g x) / (tofunction h x))
tofunction (NodeU f g)   = case f of
                             Sin -> (\x -> sin $ tofunction g x)
                             Cos -> (\x -> cos $ tofunction g x)
                             Exp -> (\x -> exp $ tofunction g x)
                             Sigmoid -> (\x -> sigmoid $ tofunction g x)
                             Minus -> (\x -> -(tofunction g x))
                             Identity -> (\x -> tofunction g x)
tofunction (NodeT f)     = case f of
                             Sin -> (\x -> sin x)
                             Cos -> (\x -> cos x)
                             Exp -> (\x -> exp x)
                             Sigmoid -> (\x -> sigmoid x)
                             Minus -> (\x -> -x)
                             Identity -> (\x -> x)
tofunction (NodeC y)     = \_ -> y
tofunction (If c g h) = \x -> if (tofunction c x) > 0.0
                                 then tofunction g x
                                 else tofunction h x
                                 


(|+|) g h = NodeB Add g h
(|*|) g h = NodeB Mul g h
(|-|) g h = NodeB Sub g h
(|/|) g h = NodeB Div g h
gsin g= NodeU Sin g
gcos g= NodeU Cos g
gid = NodeT Identity
gmax g h=If (NodeB Sub g h) g h
gmin g h=If (NodeB Sub g h) h g
gconstant x=NodeC x
gsigmoid g=NodeU Sigmoid g
gsigmoid2 g =NodeB Div (NodeC 1.0) (NodeB Add (NodeC 1.0) (NodeU Exp (NodeU Minus g)))

