import "string"
import "list"
import "map"

// methods and len() called straight on an element of a class with __getitem__/__setitem__
// (used to leave the target unresolved and crash the compiler)
function main() -> int32:
    let names = List[String]()
    defer names.free()
    names.push(String("ada"))
    names.push(String("grace"))
    let shout = names[1].upper()
    defer shout.free()
    print(names[0].c_str(), len(names[1]), shout.c_str())
    for i in 0..len(names):
        print(i, names[i].c_str(), names[i].starts_with("g"))

    // assignments through [] still become __setitem__
    let nums = List[int]()
    defer nums.free()
    nums.push(1)
    nums.push(2)
    nums[0] = 10
    nums[1] += 5
    nums[nums[1] - 7] *= 3
    print(nums[0], nums[1])

    let ages = Map[str, int]()
    defer ages.free()
    ages["ada"] = 36
    ages["ada"] += 1
    print(ages["ada"])
    return 0

// expect:
// ada 5 GRACE
// 0 ada false
// 1 grace true
// 30 7
// 37
