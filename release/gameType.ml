open Definition
open Constant

type maprecord = {
  hexes: hex list;
  ports: port list
}

type structurerecord = {
  settlements: intersection list;
  roads: road list
}

type boardrecord = {
  map: maprecord;
  structures: structurerecord;
  deck: cards;
  discard: card list;
  robber: piece
}

type resourcerecord = {
  mutable bricks: int;
  mutable wool: int;
  mutable ore: int;
  mutable grain: int;
  mutable lumber: int
}

type playerrecord = {
  inventory: resourcerecord;
  cards: cards;
  knights: int;
  longestroad: bool;
  largestarmy: bool;
  ratio: resourcerecord
}

type nextrecord = {
  color: color;
  request: request
}

type t = {
  board: boardrecord;
  blue: playerrecord;
  red: playerrecord;
  orange: playerrecord;
  white: playerrecord;
  turn: turn;
  next: color * request
}