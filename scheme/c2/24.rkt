(list 1 (list 2 (list 3 4)))
(list 1 (list 2 (cons (3, cons (4, nil)))))
(list 1 (cons (2, cons (3, cons (4, nil)))))
(cons (1, (cons (2, cons (3, cons (4, nil))))))

(list 1 moar)
(cons 1 (cons moar nil))

--> [*][*]->[*][/]
     |       |
    [1]     [m]

--> [*][*]->[*][/]
     |       |
    [1]     [*][*]->[*][/]
             |       |
            [2]     [*][*]->[*][*]
                     |       |
                    [3]     [*][/]
                             |
                            [4]
