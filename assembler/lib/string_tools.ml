let rec trim_left ~rprefix (s : string) : string = 
        if (String.contains rprefix s.[0]) then 
                let length = String.length s in
                let reduced_string = (String.sub s 1 (length - 1)) in
                trim_left ~rprefix reduced_string
        else 
                s

                
let rec trim_right ~rsuffix (s : string) : string = 
        let length = String.length s in
        if (String.contains rsuffix s.[length - 1]) then
                let reduced_string = (String.sub s 0 (length - 1)) in
                trim_right ~rsuffix reduced_string
        else 
                s

let trim ~rprefix ~rsuffix (s : string) : string = 
        let left_trimmed = trim_left ~rprefix:rprefix s in
        trim_right ~rsuffix:rsuffix left_trimmed