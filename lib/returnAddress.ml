let address_counter = ref 1

let prefix = "RET_ADDRESS_CALL"

let generate () : string = 
    let address = Printf.sprintf "%s%d" prefix !address_counter in
    incr address_counter;
    address

let bootstrap = Printf.sprintf "%s%d" prefix 0 