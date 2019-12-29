!0                ; mark the start
Rou               ; read first number
[                 ; if it's not zero
    >1roRo        ; read the second number
    u<1u          ; push input numbers to the stack
    [-1>1]uWooo   ; set value
    ?0-1[
        o0<1?0-1
    ]             ; reset
    o0roRo
]