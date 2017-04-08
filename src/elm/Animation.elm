module Animation exposing (step, linear, easeOut, easeIn, combine, inAndOut)


step : Float -> Float -> Float -> Float -> Float
step start end amount now =
    if now > start && now < end then
        amount
    else
        0


linear : Float -> Float -> Float
linear amount now =
    amount * now


easeOut : Float -> Float -> Float
easeOut amount now =
    amount * sqrt now


easeIn : Float -> Float -> Float
easeIn amount now =
    amount * now * now


combine : Float -> (Float -> Float) -> (Float -> Float) -> Float -> Float
combine ratio animation1 animation2 now =
    if now < ratio then
        animation1 (now / ratio)
    else
        (animation1 1) + animation2 ((now - ratio) / (1 - ratio))


inAndOut : Float -> (Float -> Float -> Float) -> Float -> Float -> Float
inAndOut ratio animation amount now =
    combine ratio (animation amount) (animation -amount) now
