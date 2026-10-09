// N4: an override is called through the base's slot, so it must have the same types (a String result where
// the base gives a str would be read as the wrong value and leak)
class Base:
    id: int

    operator str(self) -> str:
        return "base"

class D(Base):
    tag: String

    operator str(self) -> String:
        return "D " + self.tag

function main() -> int32:
    let d = D(1, String("x"))
    print(d)
    return 0

// expect-error: An override does not match the method it replaces
