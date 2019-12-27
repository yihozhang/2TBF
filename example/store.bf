!0                ; mark the start
Ru                ; read first number
[                 ; if it's not zero
    >1rR          ; read the second number
    u<1u          ; push input numbers to the stack
    [-1>1]Woo     ; set value
    ?0-1[
        o0<1?0-1
    ]             ; reset
    o0rR
]