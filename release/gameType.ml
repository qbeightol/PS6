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
  bricks: int;
  wool: int;
  ore: int;
  grain: int;
  lumber: int
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