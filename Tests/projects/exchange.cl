// Stress-test regression: an order-matching engine (three-level inheritance with
// overridden methods and properties through base pointers, derived constructors with
// positional + keyword arguments, Map[String, class] field updates, variants, generators).
// Order-matching engine: limit / market / iceberg / stop orders, price-time priority,
// partial fills, cancels, a trade log and an end-of-day report.
import "math"
import "memory"

enum Side:
    Buy
    Sell

enum Event:
    Limit(trader: String, side: Side, qty: int, price: int)
    Market(trader: String, side: Side, qty: int)
    Iceberg(trader: String, side: Side, qty: int, price: int, peak: int)
    Stop(trader: String, side: Side, qty: int, trigger: int)
    Cancel(id: int)
    Close

trait Reportable:
    function line(self: *Reportable) -> String

// cents -> "12.34"
function money(cents: int64) -> String:
    let s = String()
    if cents < 0:
        s += "-"
        cents = -cents
    s.append_int(cents / 100)
    s += "."
    let c = cents % 100
    if c < 10:
        s += "0"
    s.append_int(c)
    return s

function side_name(s: Side) -> str:
    return when s == Side.Buy use "BUY" otherwise "SELL"

class Order(Reportable):
    id: int
    trader: String
    side: Side
    qty: int
    filled: int = 0
    seq: int = 0
    notes: List[String]

    function kind(self) -> str:
        return "order"

    function limit(self) -> int:
        return 0

    function can_rest(self) -> bool:
        return true

    // how much of the order the book shows
    property visible(self) -> int:
        return self.remaining

    function crosses(self, price: int) -> bool:
        if self.side == Side.Buy:
            return price <= self.limit()
        return price >= self.limit()

    property remaining(self) -> int:
        return self.qty - self.filled

    function line(self) -> String:
        let s = String("#")
        s.append_int(self.id)
        s += " " + self.trader + " " + side_name(self.side) + " " + self.kind() + " "
        s.append_int(self.filled)
        s += "/"
        s.append_int(self.qty)
        s += " @ " + money(self.limit())
        return s

class LimitOrder(Order):
    price: int

    function kind(self) -> str:
        return "limit"

    function limit(self) -> int:
        return self.price

class MarketOrder(Order):
    function kind(self) -> str:
        return "market"

    function limit(self) -> int:
        return when self.side == Side.Buy use 1_000_000_000 otherwise 0

    function can_rest(self) -> bool:
        return false

class IcebergOrder(LimitOrder):
    peak: int

    function kind(self) -> str:
        return "iceberg"

    property visible(self) -> int:
        return min(self.peak, self.remaining)

class Trade(Reportable):
    buy_id: int
    sell_id: int
    buyer: String
    seller: String
    price: int
    qty: int

    function line(self) -> String:
        let s = String()
        s.append_int(self.qty)
        s += " @ " + money(self.price) + "  " + self.buyer + " <- " + self.seller
        return s

class Position:
    shares: int = 0
    cash: int64 = 0
    trades: int = 0

class Filled:
    id: int
    qty: int
    trades: int

class Rested:
    id: int
    shown: int

variant Ack:
    Filled
    Rested
    String

class Book:
    bids: List[*Order]
    asks: List[*Order]
    stops: List[(int, *Order)]
    by_id: Map[int, *Order]
    trades: List[Trade]
    positions: Map[String, Position]
    next_id: int = 1
    clock: int = 0
    last_price: int = 0
    rejected: int = 0

    function side_of(self, s: Side) -> *List[*Order]:
        return when s == Side.Buy use &self.bids otherwise &self.asks

    function resort(self, s: Side):
        if s == Side.Buy:
            self.bids.sort_by(lambda o: -(o.limit() as int64) * 1_000_000 + o.seq as int64)
        else:
            self.asks.sort_by(lambda o: (o.limit() as int64) * 1_000_000 + o.seq as int64)

    function book_trade(self, taker: *Order, maker: *Order, qty: int):
        let price = maker.limit()
        let buy = when taker.side == Side.Buy use taker otherwise maker
        let sell = when taker.side == Side.Buy use maker otherwise taker
        self.trades.push(Trade(buy.id, sell.id, buy.trader, sell.trader, price, qty))
        taker.filled += qty
        maker.filled += qty
        self.last_price = price
        self.positions[buy.trader].shares += qty
        self.positions[buy.trader].cash -= price as int64 * qty as int64
        self.positions[buy.trader].trades++
        self.positions[sell.trader].shares -= qty
        self.positions[sell.trader].cash += price as int64 * qty as int64
        self.positions[sell.trader].trades++

    function remove_order(self, o: *Order):
        let side = self.side_of(o.side)
        if i := side.index_of(o):
            side.remove(i)
        self.by_id.remove(o.id)

    function match(self, taker: *Order) -> int:
        let opposite = self.side_of(when taker.side == Side.Buy use Side.Sell otherwise Side.Buy)
        let count = 0
        while taker.remaining > 0 and len(*opposite) > 0:
            let best = opposite[0]
            if not taker.crosses(best.limit()):
                break
            let qty = min(taker.remaining, best.visible)
            self.book_trade(taker, best, qty)
            count++
            if best.remaining == 0:
                opposite.remove(0)
                self.by_id.remove(best.id)
                best.notes.push(String("done"))
                self.retire(best)
            else if best.visible > 0 and best.kind() == "iceberg":
                // a refilled iceberg goes to the back of its price level
                self.clock++
                best.seq = self.clock
                best.notes.push(String("refill"))
                self.resort(best.side)
        return count

    function ensure(self, trader: *String):
        if *trader not in self.positions:
            self.positions[*trader] = Position()

    function submit(self, o: *Order) -> Ack:
        self.ensure(&o.trader)
        self.clock++
        o.seq = self.clock
        if o.qty <= 0:
            self.rejected++
            let why = String("rejected #")
            why.append_int(o.id)
            why += ": quantity must be positive"
            self.retire(o)
            return why
        let n = self.match(o)
        let result: Ack = Filled(o.id, o.filled, n)
        if o.remaining > 0 and o.can_rest():
            let side = self.side_of(o.side)
            side.push(o)
            self.by_id[o.id] = o
            self.resort(o.side)
            result = Rested(o.id, o.visible)
        else if o.remaining > 0:
            let why = String("market #")
            why.append_int(o.id)
            why += " unfilled: "
            why.append_int(o.remaining)
            self.retire(o)
            return why
        else:
            self.retire(o)
        self.fire_stops()
        return result

    function fire_stops(self):
        let i: int64 = 0
        while i < len(self.stops):
            let trigger, o = self.stops[i]
            let hit = when o.side == Side.Buy use self.last_price >= trigger otherwise self.last_price <= trigger
            if hit and self.last_price > 0:
                self.stops.remove(i)
                o.notes.push(String("triggered"))
                let ack = self.submit(o)
                print("  stop", trigger, "fired:", describe(&ack))
            else:
                i++

    function cancel(self, id: int) -> bool:
        if o := self.by_id.get(id):
            self.remove_order(o)
            self.retire(o)
            return true
        return false

    // orders that left the book are kept until the end of the day, then freed
    retired: List[*Order]

    function retire(self, o: *Order):
        self.retired.push(o)

    function make_id(self) -> int:
        let id = self.next_id
        self.next_id++
        return id

    operator destruct(self):
        for o in self.bids:
            self.retired.push(o)
        for o in self.asks:
            self.retired.push(o)
        for entry in self.stops:
            self.retired.push(entry[1])
        for o in self.retired:
            destroy(o)
            release(o)

function describe(a: *Ack) -> String:
    switch *a:
        case Filled(f):
            let s = String("filled #")
            s.append_int(f.id)
            s += " qty "
            s.append_int(f.qty)
            s += " in "
            s.append_int(f.trades)
            s += " trades"
            return s
        case Rested(r):
            let s = String("rested #")
            s.append_int(r.id)
            s += " showing "
            s.append_int(r.shown)
            return s
        case String(msg):
            return msg

function print_all[T: Reportable](title: str, items: *List[T]):
    print(title, len(*items))
    for it in *items:
        print("  ", it.line())

function feed() -> Generator[Event]:
    yield Event.Limit(String("ann"), Side.Sell, 100, 10_100)
    yield Event.Limit(String("bob"), Side.Sell, 50, 10_050)
    yield Event.Iceberg(String("cat"), Side.Sell, 300, 10_100, 100)
    yield Event.Limit(String("dan"), Side.Buy, 80, 9_900)
    yield Event.Limit(String("eve"), Side.Buy, 120, 9_950)
    yield Event.Stop(String("fay"), Side.Buy, 60, 10_100)
    yield Event.Limit(String("gus"), Side.Buy, 120, 10_100)
    yield Event.Market(String("hal"), Side.Sell, 150)
    yield Event.Cancel(4)
    yield Event.Cancel(99)
    yield Event.Limit(String("ann"), Side.Buy, 0, 10_000)
    yield Event.Market(String("bob"), Side.Buy, 500)
    yield Event.Limit(String("dan"), Side.Sell, 40, 9_800)
    yield Event.Close
    yield Event.Limit(String("zed"), Side.Buy, 1, 1)

function make_order[T](o: T) -> *Order:
    let p = allocate[T](1)
    place(p, o)
    return p

function main() -> int32:
    let book = Book()
    let events = 0
    for ev in feed():
        events++
        let o: ?*Order = none
        switch ev:
            case Limit(trader, side, qty, price):
                o = make_order(LimitOrder(book.make_id(), trader, side, qty, price = price))
            case Market(trader, side, qty):
                o = make_order(MarketOrder(book.make_id(), trader, side, qty))
            case Iceberg(trader, side, qty, price, peak):
                o = make_order(IcebergOrder(book.make_id(), trader, side, qty, price = price, peak = peak))
            case Stop(trader, side, qty, trigger):
                let stop = make_order(MarketOrder(book.make_id(), trader, side, qty))
                book.ensure(&stop.trader)
                book.stops.push((trigger, stop))
                print("stop #" + from_int(stop.id), side_name(side), qty, "at", money(trigger))
            case Cancel(id):
                print("cancel", id, "->", book.cancel(id))
            case Close:
                print("market closed after", events, "events")
                break
        if order := o:
            print("in:", order.line())
            let ack = book.submit(order)
            print("  ->", describe(&ack))

    print_all("trades", &book.trades)
    print_all("bids", &book.bids)
    print_all("asks", &book.asks)

    let names = List[String]()
    for name in book.positions:
        names.push(name)
    names.sort()
    let net_shares = 0
    let net_cash: int64 = 0
    for name in names:
        net_shares += book.positions[name].shares
        net_cash += book.positions[name].cash
        print(name, "shares", book.positions[name].shares, "cash", money(book.positions[name].cash), "trades", book.positions[name].trades)
    print("net", net_shares, money(net_cash), "last", money(book.last_price), "rejected", book.rejected)
    let volume = 0
    let notional: int64 = 0
    for t in book.trades:
        volume += t.qty
        notional += t.price as int64 * t.qty as int64
    print("volume", volume, "vwap", money(notional / volume as int64))
    let big = book.trades.filter(lambda t: t.qty >= 100).map(lambda t: t.qty)
    print("big trades", big)
    return 0

// expect:
// in: #1 ann SELL limit 0/100 @ 101.00
//   -> rested #1 showing 100
// in: #2 bob SELL limit 0/50 @ 100.50
//   -> rested #2 showing 50
// in: #3 cat SELL iceberg 0/300 @ 101.00
//   -> rested #3 showing 100
// in: #4 dan BUY limit 0/80 @ 99.00
//   -> rested #4 showing 80
// in: #5 eve BUY limit 0/120 @ 99.50
//   -> rested #5 showing 120
// stop #6 BUY 60 at 101.00
// in: #7 gus BUY limit 0/120 @ 101.00
//   stop 10100 fired: filled #6 qty 60 in 2 trades
//   -> filled #7 qty 120 in 2 trades
// in: #8 hal SELL market 0/150 @ 0.00
//   -> filled #8 qty 150 in 2 trades
// cancel 4 -> true
// cancel 99 -> false
// in: #9 ann BUY limit 0/0 @ 100.00
//   -> rejected #9: quantity must be positive
// in: #10 bob BUY market 0/500 @ 10000000.00
//   -> market #10 unfilled: 230
// in: #11 dan SELL limit 0/40 @ 98.00
//   -> rested #11 showing 40
// market closed after 14 events
// trades 9
//    50 @ 100.50  gus <- bob
//    70 @ 101.00  gus <- ann
//    30 @ 101.00  fay <- ann
//    30 @ 101.00  fay <- cat
//    120 @ 99.50  eve <- hal
//    30 @ 99.00  dan <- hal
//    100 @ 101.00  bob <- cat
//    100 @ 101.00  bob <- cat
//    70 @ 101.00  bob <- cat
// bids 0
// asks 1
//    #11 dan SELL limit 0/40 @ 98.00
// ann shares -100 cash 10100.00 trades 2
// bob shares 220 cash -22245.00 trades 4
// cat shares -300 cash 30300.00 trades 4
// dan shares 30 cash -2970.00 trades 1
// eve shares 120 cash -11940.00 trades 1
// fay shares 60 cash -6060.00 trades 2
// gus shares 120 cash -12095.00 trades 2
// hal shares -150 cash 14910.00 trades 2
// net 0 0.00 last 101.00 rejected 1
// volume 600 vwap 100.55
// big trades [120, 100, 100]
