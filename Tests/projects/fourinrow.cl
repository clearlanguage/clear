// Stress-test regression: connect four with alpha-beta search, two async player tasks
// on a round-robin scheduler, generators of legal moves, derived constructors with
// keyword arguments, and a tic-tac-toe minimax through a *[9; int8].
// Connect four: an alpha-beta minimax AI, a scripted opponent, two async player tasks
// driven by a round-robin scheduler, and a generator of legal moves. Then a tic-tac-toe
// self-play check (perfect play from a fixed opening must draw).
import "math"

const ROWS = 6
const COLS = 7
const CELLS = ROWS * COLS
const WIN_SCORE = 1_000_000

enum Result:
    Playing
    Win(mark: int8, moves: int)
    Draw(moves: int)

class Board:
    cells: [CELLS; int8]
    heights: [COLS; int]
    moves: int = 0
    history: List[int]

    function at(self, r: int, c: int) -> int8:
        return self.cells[r * COLS + c]

    function can_play(self, c: int) -> bool:
        return c >= 0 and c < COLS and self.heights[c] < ROWS

    function play(self, c: int, mark: int8):
        let r = self.heights[c]
        self.cells[r * COLS + c] = mark
        self.heights[c]++
        self.moves++
        self.history.push(c)

    function undo(self):
        let c = self.history.pop()
        self.heights[c]--
        self.cells[self.heights[c] * COLS + c] = 0
        self.moves--

    // does the last stone in column c make four?
    function wins_at(self, c: int) -> bool:
        let r = self.heights[c] - 1
        let mark = self.at(r, c)
        for dir in [(0, 1), (1, 0), (1, 1), (1, -1)]:
            let dr, dc = dir
            let n = 1
            for sign in [1, -1]:
                let rr = r + dr * sign
                let cc = c + dc * sign
                while rr >= 0 and rr < ROWS and cc >= 0 and cc < COLS and self.at(rr, cc) == mark:
                    n++
                    rr += dr * sign
                    cc += dc * sign
            if n >= 4:
                return true
        return false

    function render(self) -> String:
        let s = String()
        let r = ROWS - 1
        while r >= 0:
            s += "|"
            for c in 0..COLS:
                let m = self.at(r, c)
                s.push(when m == 1 use 'X' otherwise when m == 2 use 'O' otherwise '.')
            s += "|\n"
            r--
        s += "+0123456+"
        return s

// centre columns first: better moves are tried first, so alpha-beta cuts more
function legal_moves(b: *Board) -> Generator[int]:
    for c in [3, 2, 4, 1, 5, 0, 6]:
        if b.can_play(c):
            yield c

function window_score(b: *Board, mark: int8) -> int:
    let score = 0
    let other: int8 = 3 - mark
    for r in 0..ROWS:
        for c in 0..COLS:
            for dir in [(0, 1), (1, 0), (1, 1), (-1, 1)]:
                let dr, dc = dir
                let er = r + 3 * dr
                let ec = c + 3 * dc
                if er < 0 or er >= ROWS or ec >= COLS:
                    continue
                let mine = 0
                let theirs = 0
                for k in 0..4:
                    let v = b.at(r + k * dr, c + k * dc)
                    if v == mark:
                        mine++
                    else if v == other:
                        theirs++
                if theirs == 0:
                    score += when mine == 3 use 50 otherwise when mine == 2 use 5 otherwise 0
                if mine == 0:
                    score -= when theirs == 3 use 50 otherwise when theirs == 2 use 5 otherwise 0
    for r in 0..ROWS:
        let v = b.at(r, 3)
        if v == mark:
            score += 3
        else if v == other:
            score -= 3
    return score

class Stats:
    nodes: int64 = 0
    cutoffs: int64 = 0

// negamax with alpha-beta; returns (score, column)
function negamax(b: *Board, mark: int8, depth: int, alpha: int, beta: int, stats: *Stats) -> (int, int):
    stats.nodes++
    if b.moves == CELLS:
        return 0, -1
    if depth == 0:
        return window_score(b, mark), -1
    let best = -WIN_SCORE * 10
    let best_col = -1
    for c in legal_moves(b):
        b.play(c, mark)
        let score = 0
        if b.wins_at(c):
            score = WIN_SCORE + depth
        else:
            let s, unused = negamax(b, 3 - mark, depth - 1, -beta, -alpha, stats)
            score = -s
        b.undo()
        if score > best:
            best = score
            best_col = c
        alpha = max(alpha, score)
        if alpha >= beta:
            stats.cutoffs++
            break
    return best, best_col

class Player:
    name: String
    mark: int8

    function choose(self, b: *Board) -> int:
        for c in legal_moves(b):
            return c
        return -1

    function describe(self) -> String:
        return self.name + " (" + (when self.mark == 1 use "X" otherwise "O") + ")"

class ScriptedPlayer(Player):
    script: List[int]
    next: int64 = 0

    function choose(self, b: *Board) -> int:
        while self.next < len(self.script):
            let c = self.script[self.next]
            self.next++
            if b.can_play(c):
                return c
        return super.choose(b)

class AiPlayer(Player):
    depth: int
    stats: Stats

    function choose(self, b: *Board) -> int:
        let score, col = negamax(b, self.mark, self.depth, -WIN_SCORE * 10, WIN_SCORE * 10, &self.stats)
        return col

class Game:
    board: Board
    turn: int8 = 1
    result: Result = Result.Playing
    log: List[String]

async function player_task(p: *Player, g: *Game) -> int:
    let mine = 0
    while g.result is Result.Playing:
        if g.turn != p.mark:
            await pause()
            continue
        let c = p.choose(&g.board)
        g.board.play(c, p.mark)
        mine++
        let line = p.describe()
        line += " -> column " + from_int(c)
        g.log.push(line)
        if g.board.wins_at(c):
            g.result = Result.Win(p.mark, g.board.moves)
        else if g.board.moves == CELLS:
            g.result = Result.Draw(g.board.moves)
        g.turn = 3 - g.turn
        await pause()
    return mine

function play_match(a: *Player, b: *Player) -> Result:
    let g = Game()
    let tasks = List[Task[int]]()
    tasks.push(player_task(a, &g))
    tasks.push(player_task(b, &g))
    let rounds = 0
    let running = true
    while running:
        running = false
        for t in tasks:
            if not t.done():
                t.resume()
                running = true
        rounds++
    for line in g.log:
        print(" ", line)
    print(g.board.render())
    print("moves by player:", tasks[0].result(), tasks[1].result(), "scheduler rounds:", rounds)
    return g.result

function show(r: Result) -> String:
    switch r:
        case Playing:
            return String("still playing")
        case Win(mark, moves):
            return String(when mark == 1 use "X" otherwise "O") + " wins after " + from_int(moves) + " moves"
        case Draw(moves):
            return "draw after " + from_int(moves) + " moves"

// ---------- tic-tac-toe: full-depth minimax, must always draw ----------
function ttt_winner(b: *[9; int8]) -> int8:
    for line in [(0, 1, 2), (3, 4, 5), (6, 7, 8), (0, 3, 6), (1, 4, 7), (2, 5, 8), (0, 4, 8), (2, 4, 6)]:
        let x, y, z = line
        if b[x] != 0 and b[x] == b[y] and b[y] == b[z]:
            return b[x]
    return 0

function ttt_minimax(b: *[9; int8], mark: int8, counter: *int64) -> (int, int):
    *counter += 1
    let w = ttt_winner(b)
    if w != 0:
        return when w == mark use 1 otherwise -1, -1
    let best = -2
    let best_move = -1
    for i in 0..9:
        if b[i] == 0:
            b[i] = mark
            let s, unused = ttt_minimax(b, 3 - mark, counter)
            b[i] = 0
            if -s > best:
                best = -s
                best_move = i
    if best_move == -1:
        return 0, -1
    return best, best_move

function main() -> int32:
    let human = ScriptedPlayer("script", 1)
    for c in {3, 3, 4, 5, 2, 2, 6, 0, 0, 1}:
        human.script.push(c)
    let ai = AiPlayer("minimax", 2, depth = 3)
    print("match 1:", human.describe(), "vs", ai.describe())
    print(show(play_match(&human, &ai)))
    print("ai searched", ai.stats.nodes, "nodes,", ai.stats.cutoffs, "cutoffs")

    let deep = AiPlayer("deep", 1, depth = 4)
    let shallow = AiPlayer("shallow", 2, depth = 2)
    print("match 2:", deep.describe(), "vs", shallow.describe())
    print(show(play_match(&deep, &shallow)))
    print("nodes", deep.stats.nodes, shallow.stats.nodes)

    // start from X in the centre and O in a corner, to keep the search small
    let board: [9; int8] = {}
    board[4] = 1
    board[0] = 2
    let mark: int8 = 1
    let total_nodes: int64 = 0
    let moves = List[int]()
    moves.push(4)
    moves.push(0)
    while ttt_winner(&board) == 0 and len(moves) < 9:
        let score, m = ttt_minimax(&board, mark, &total_nodes)
        board[m] = mark
        moves.push(m)
        mark = 3 - mark
    print("tic-tac-toe:", moves, "winner", ttt_winner(&board), "nodes", total_nodes)
    return 0

// expect:
// match 1: script (X) vs minimax (O)
//   script (X) -> column 3
//   minimax (O) -> column 3
//   script (X) -> column 3
//   minimax (O) -> column 3
//   script (X) -> column 4
//   minimax (O) -> column 2
//   script (X) -> column 5
//   minimax (O) -> column 6
//   script (X) -> column 2
//   minimax (O) -> column 5
//   script (X) -> column 2
//   minimax (O) -> column 4
//   script (X) -> column 6
//   minimax (O) -> column 4
// |.......|
// |.......|
// |...O...|
// |..XXO..|
// |..XOOOX|
// |..OXXXO|
// +0123456+
// moves by player: 7 7 scheduler rounds: 9
// O wins after 14 moves
// ai searched 1093 nodes, 123 cutoffs
// match 2: deep (X) vs shallow (O)
//   deep (X) -> column 3
//   shallow (O) -> column 2
//   deep (X) -> column 3
//   shallow (O) -> column 3
//   deep (X) -> column 4
//   shallow (O) -> column 5
//   deep (X) -> column 4
//   shallow (O) -> column 2
//   deep (X) -> column 2
//   shallow (O) -> column 1
//   deep (X) -> column 3
//   shallow (O) -> column 1
//   deep (X) -> column 2
//   shallow (O) -> column 5
//   deep (X) -> column 5
//   shallow (O) -> column 5
//   deep (X) -> column 3
//   shallow (O) -> column 5
//   deep (X) -> column 1
//   shallow (O) -> column 1
//   deep (X) -> column 2
//   shallow (O) -> column 2
//   deep (X) -> column 1
//   shallow (O) -> column 1
//   deep (X) -> column 3
//   shallow (O) -> column 5
//   deep (X) -> column 0
//   shallow (O) -> column 0
//   deep (X) -> column 0
//   shallow (O) -> column 6
//   deep (X) -> column 6
//   shallow (O) -> column 4
//   deep (X) -> column 4
// |.OOX.O.|
// |.XXX.O.|
// |.OXXXO.|
// |XXXOOX.|
// |OOOXXOX|
// |XOOXXOO|
// +0123456+
// moves by player: 17 16 scheduler rounds: 19
// X wins after 33 moves
// nodes 4645 437
// tic-tac-toe: [4, 0, 1, 7, 3, 5, 2, 6, 8] winner 0 nodes 8199
