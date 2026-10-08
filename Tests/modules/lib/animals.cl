class Animal:
    name: str

    virtual function sound(self: *Animal) -> str:
        return "..."

    function speak(self: *Animal):
        print(self.name, "says", self.sound())
