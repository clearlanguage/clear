// no import needed: String is always available, and a literal becomes a String where that is the type

function main() -> int32:
    let x:String = "hello "
    let y:String = ",world"

    let xy = x + y    
    print(xy)

    return 0

// expect:
// hello ,world
