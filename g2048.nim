import std/[terminal, strutils, strformat, random, parseopt, os]

type Cell = ref object
  value: int = 0
  merged: bool = false

type Map[W, H: static[int]] = array[0..(H-1), array[0..(W-1), Cell]]

type Direction = enum
  dLeft
  dRight
  dUp
  dDown

randomize()

var score = 0

var options: tuple[debug: bool, nosleep: bool]

func hide(v: int): string =
  if v != 0:
    $v
  else:
    " "

template debug(body: untyped) =
  if options.debug:
    echo body

template debugmap(map: Map) =
  if options.debug:
    let buffer = map.debugToString
    discard stdout.writeBuffer(cstring(buffer), len(buffer))

template output(map: Map) =
  if not options.debug:
    stdout.eraseScreen()
    setCursorPos(0, 0)
  let buffer = &"score: {score}\n" & map.toString
  discard stdout.writeBuffer(cstring(buffer), len(buffer))

func `[]`(map: Map, xy: (int, int)): Cell =
  array(map)[xy[1]][xy[0]]

func `[]`(map: Map, x, y: int): Cell =
  array(map)[y][x]

proc `[]=`(map: var Map, x, y: int, v: Cell) =
  array(map)[y][x] = v

proc `[]=`(map: var Map, xy: (int, int), v: Cell) =
  array(map)[xy[1]][xy[0]] = v

func width[W, H](map: Map[W, H]): int = W

func height[W, H](map: Map[W, H]): int = H

func neighbor(map: Map, direction: Direction, x, y, n: int): (int, int) =
  # 获取滑动方向上第n个cell
  case direction
  of dLeft:
    (x-n, y)
  of dRight:
    (x+n, y)
  of dUp:
    (x, y-n)
  of dDown:
    (x, y+n)

proc movecell(map: var Map, direction: Direction, x, y: int) =
  debug &"moving x={x} y={y}"
  let cell = map[x, y]
  if cell.value == 0:
    return

  let `end` =
    case direction:
    of dLeft:
      x
    of dRight:
      map.width-x-1
    of dUp:
      y
    of dDown:
      map.height-y-1
  var merged = false
  var firstneighborindex = 0
  debug &"end={`end`}"

  for i in 1..`end`:
    let xy = map.neighbor(direction, x, y, i)
    let neighbor = map[xy]
    debug &"{i}th neighbor, x={xy[0]}, y={xy[1]}, v={neighbor.value}, merged={neighbor.merged}"

    if neighbor.value != 0: # 滑动方向上的第一个cell决定了当前cell将会如何改变
      firstneighborindex = i
      if neighbor.value == cell.value and not neighbor.merged: # 可以合
        debug "Merge"
        neighbor.value += cell.value # 合体
        score += neighbor.value # 加分
        neighbor.merged = true # 已经合体的cell不能在同一回合再次合体
        map[x, y] = Cell() # 清空原cell
        merged = true
      break

  if not merged and `end` != 0: # 如果自己不是贴着滑动方向的边界，且没有合体
    debug &"Not merged, moving. fni={firstneighborindex}"
    if firstneighborindex == 0:
      let xy = map.neighbor(direction, x, y,
          `end`) # 把自己移动到滑动方向上的边界旁
      map[xy] = map[x, y]
      map[x, y] = Cell()
    elif firstneighborindex != 1:
      let xy = map.neighbor(direction, x, y,
          firstneighborindex-1) # 把自己移动到滑动方向上第一个cell后面
      map[xy] = map[x, y]
      map[x, y] = Cell()

proc movemap(map: var Map, direction: Direction) =
  case direction
  of dLeft, dRight:
    for y in 0..<map.height: # 分别处理每一行，因为不同的行之间不会互相影响
      case direction
      of dLeft:
        for x in countup(0, map.width-1, 1):
          # 从滑动方向上最前面一个cell开始处理，因为只有前面cell的情况会影响后面cell的情况
          map.movecell(direction, x, y)
          debugmap(map)
      of dRight:
        for x in countdown(map.width-1, 0, 1):
          map.movecell(direction, x, y)
          debugmap(map)
      else:
        assert false
  of dUp, dDown:
    for x in 0..<map.width:
      case direction
      of dUp:
        for y in countup(0, map.height-1, 1):
          map.movecell(direction, x, y)
          debugmap(map)
      of dDown:
        for y in countdown(map.height-1, 0, 1):
          map.movecell(direction, x, y)
          debugmap(map)
      else:
        assert false

proc spawn(map: var Map) =
  var xys: seq[(int, int)] = @[]
  for x in 0..<map.width:
    for y in 0..<map.height:
      if map[x, y].value == 0:
        xys.add((x, y))
  xys.shuffle()
  var threshold = 0.8
  for xy in xys:
    if rand(1.0) > threshold:
      break
    map[xy].value = sample([2, 4, 8], [0.6, 0.3, 0.1])
    threshold = threshold * 0.2

proc next(map: var Map, direction: Direction) =
  map.movemap(direction)
  for x in 0..<map.width:
    for y in 0..<map.height:
      let cell = map[x, y]
      cell.merged = false # 刷新merged属性
  if not options.nosleep:
    output(map)
    sleep(200)
  map.spawn()

func debugToString(map: Map): string =
  for y in 0..<map.height:
    for x in 0..<map.width:
      result &= map[x, y].value.`$`.alignLeft(4)
    result &= "\n"

func colorize(str: string, v: int): string =
  let color =
    case v
    of 2:
      0
    of 4:
      32
    of 8:
      34
    of 16:
      35
    of 32..128:
      31
    of 256..int.high:
      33
    else:
      0
  return &"\e[{color}m{str}\e[0m"

func toString(map: Map): string =
  for y in 0..<map.height:
    result &= dedent"""
      +--------+--------+--------+--------+
      |        |        |        |        |
    """
    for x in 0..<map.width:
      result &= "|" & map[x, y].value.hide.center(8).colorize(map[x, y].value)
    result &= "|\n" & dedent """
      |        |        |        |        |
    """
  result &= dedent"""
      +--------+--------+--------+--------+
    """

proc opt(): auto =
  result = (debug: false, nosleep: false)

  for kind, key, val in getopt(longnoval = @["debug", "nosleep"]):
    debug &"Commandline argument / option: {kind=} {key=} {val=}"

    case kind

    of cmdLongOption:
      case key

      of "debug":
        result.debug = true

      of "nosleep":
        result.nosleep = true

    else:
      discard

options = opt()

var map: Map[4, 4] = [[Cell(), Cell(), Cell(), Cell()], [Cell(), Cell(), Cell(),
    Cell()], [Cell(), Cell(), Cell(), Cell()], [Cell(), Cell(), Cell(), Cell()]]

map.spawn()

while true:
  output(map)
  let direction =
    case getch()
    of 'a':
      dLeft
    of 'd':
      dRight
    of 'w':
      dUp
    of 's':
      dDown
    of 'q':
      break
    else:
      continue
  map.next(direction)
