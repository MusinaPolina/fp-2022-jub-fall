module Test.Lambda where

import Test.HUnit (Assertion, assertBool, (@?=))
import Lambda

unit_show = do
    show true @?= "λx.λy.x"
 {-    show false @?= "λx.λy.y"
    show Lambda.and @?= "λp.λq.p q p"
    show Lambda.or @?= "λp.λq.p p q"
    show ifThenElse @?= "λp.λa.λb.p a b"
    show zero @?= "λf.λx.x"
    show one @?= "λf.λx.f x"
    show two @?= "λf.λx.f (f x)"
    show three @?= "λf.λx.f (f (f x))"
    show  four @?= "λf.λx.f (f (f (f x)))"
    show add @?= "λm.λn.λf.λx.m f (n f x)"
    show successor @?= "λn.λf.λx.f (n f x)"
    show mult @?= "λm.λn.λf.m (n f)" -}

