unit uWall;

interface

uses
  uVMath;

type
  TWallVertex = record
    pos: Vec3;
    tc: Vec3;
    norm: Vec3;
  end;

const
  WALLVERTICES: array[0..87] of TWallVertex = (
  (pos: (0, 0, 0); tc: (0, 0, 1); norm: (-1, 0, 0)),
  (pos: (0, 3, 0); tc: (0, 3, 1); norm: (-1, 0, 0)),
  (pos: (0, 0, 1.531); tc: (1.531, 0, 1); norm: (-1, 0, 0)),
  (pos: (0, 3, 1.531); tc: (1.531, 3, 1); norm: (-1, 0, 0)),

  (pos: (0, 0, 1.531); tc: (0, 0, 1); norm: (0, 0, 1)),
  (pos: (0, 3, 1.531); tc: (0, 3, 1); norm: (0, 0, 1)),
  (pos: (1.931, 0, 1.531); tc: (1.931, 0, 1); norm: (0, 0, 1)),
  (pos: (1.931, 3, 1.531); tc: (1.931, 3, 1); norm: (0, 0, 1)),

  (pos: (1.931, 0, 1.531); tc: (1.931, 0, 1); norm: (0, 0, 1)),
  (pos: (1.931, 0.9, 1.531); tc: (1.931, 0.9, 1); norm: (0, 0, 1)),
  (pos: (3.131, 0, 1.531); tc: (3.131, 0, 1); norm: (0, 0, 1)),
  (pos: (3.131, 0.9, 1.531); tc: (3.131, 0.9, 1); norm: (0, 0, 1)),

  (pos: (1.931, 2.1, 1.531); tc: (1.931, 2.1, 1); norm: (0, 0, 1)),
  (pos: (1.931, 3.0, 1.531); tc: (1.931, 3.0, 1); norm: (0, 0, 1)),
  (pos: (3.131, 2.1, 1.531); tc: (3.131, 2.1, 1); norm: (0, 0, 1)),
  (pos: (3.131, 3.0, 1.531); tc: (3.131, 3.0, 1); norm: (0, 0, 1)),

  (pos: (3.131, 0, 1.531); tc: (3.131, 0, 1); norm: (0, 0, 1)),
  (pos: (3.131, 3, 1.531); tc: (3.131, 3, 1); norm: (0, 0, 1)),
  (pos: (4.531, 0, 1.531); tc: (4.531, 0, 1); norm: (0, 0, 1)),
  (pos: (4.531, 3, 1.531); tc: (4.531, 3, 1); norm: (0, 0, 1)),

  (pos: (4.531, 0, 1.531); tc: (4.531, 0, 1); norm: (1, 0, 0)),
  (pos: (4.531, 3, 1.531); tc: (4.531, 3, 1); norm: (1, 0, 0)),
  (pos: (4.531, 0, 1.0); tc: (4.531, 0, 0); norm: (1, 0, 0)),
  (pos: (4.531, 3, 1.0); tc: (4.531, 3, 0); norm: (1, 0, 0)),

  (pos: (0.531, 0, 1.0); tc: (0.531, 0, 0); norm: (0, 0, -1)),
  (pos: (0.531, 3, 1.0); tc: (0.531, 3, 0); norm: (0, 0, -1)),
  (pos: (1.931, 0, 1.0); tc: (1.931, 0, 0); norm: (0, 0, -1)),
  (pos: (1.931, 3, 1.0); tc: (1.931, 3, 0); norm: (0, 0, -1)),

  (pos: (1.931, 0, 1.0); tc: (1.931, 0, 0); norm: (0, 0, -1)),
  (pos: (1.931, 0.9, 1.0); tc: (1.931, 0.9, 0); norm: (0, 0, -1)),
  (pos: (3.131, 0, 1.0); tc: (3.131, 0, 0); norm: (0, 0, -1)),
  (pos: (3.131, 0.9, 1.0); tc: (3.131, 0.9, 0); norm: (0, 0, -1)),

  (pos: (1.931, 2.1, 1.0); tc: (1.931, 2.1, 0); norm: (0, 0, -1)),
  (pos: (1.931, 3.0, 1.0); tc: (1.931, 3.0, 0); norm: (0, 0, -1)),
  (pos: (3.131, 2.1, 1.0); tc: (3.131, 2.1, 0); norm: (0, 0, -1)),
  (pos: (3.131, 3.0, 1.0); tc: (3.131, 3.0, 0); norm: (0, 0, -1)),

  (pos: (3.131, 0, 1.0); tc: (3.131, 0, 0); norm: (0, 0, -1)),
  (pos: (3.131, 3, 1.0); tc: (3.131, 3, 0); norm: (0, 0, -1)),
  (pos: (4.531, 0, 1.0); tc: (4.531, 0, 0); norm: (0, 0, -1)),
  (pos: (4.531, 3, 1.0); tc: (4.531, 3, 0); norm: (0, 0, -1)),

  (pos: (0.531, 0, 0); tc: (0, 0, 0); norm: (1, 0, 0)),
  (pos: (0.531, 3, 0); tc: (0, 3, 0); norm: (1, 0, 0)),
  (pos: (0.531, 0, 1.0); tc: (1.0, 0, 0); norm: (1, 0, 0)),
  (pos: (0.531, 3, 1.0); tc: (1.0, 3, 0); norm: (1, 0, 0)),

  (pos: (0, 0, 0); tc: (0, 0, 1); norm: (0, 0, -1)),
  (pos: (0, 3, 0); tc: (0, 3, 1); norm: (0, 0, -1)),
  (pos: (0.531, 0, 0); tc: (0, 0, 0); norm: (0, 0, -1)),
  (pos: (0.531, 3, 0); tc: (0, 3, 0); norm: (0, 0, -1)),

  (pos: (1.931, 0.9, 1.0); tc: (1.931, 0.9, 0); norm: (1, 0, 0)),
  (pos: (1.931, 2.1, 1.0); tc: (1.931, 2.1, 0); norm: (1, 0, 0)),
  (pos: (1.931, 0.9, 1.531); tc: (1.931, 0.9, 1); norm: (1, 0, 0)),
  (pos: (1.931, 2.1, 1.531); tc: (1.931, 2.1, 1); norm: (1, 0, 0)),

  (pos: (3.131, 0.9, 1.0); tc: (3.131, 0.9, 0); norm: (-1, 0, 0)),
  (pos: (3.131, 2.1, 1.0); tc: (3.131, 2.1, 0); norm: (-1, 0, 0)),
  (pos: (3.131, 0.9, 1.531); tc: (3.131, 0.9, 1); norm: (-1, 0, 0)),
  (pos: (3.131, 2.1, 1.531); tc: (3.131, 2.1, 1); norm: (-1, 0, 0)),

  (pos: (1.931, 0.9, 1.0); tc: (1.931, 0.9, 0); norm: (0, 1, 0)),
  (pos: (1.931, 0.9, 1.531); tc: (1.931, 0.9, 1); norm: (0, 1, 0)),
  (pos: (3.131, 0.9, 1.0); tc: (3.131, 0.9, 0); norm: (0, 1, 0)),
  (pos: (3.131, 0.9, 1.531); tc: (3.131, 0.9, 1); norm: (0, 1, 0)),

  (pos: (1.931, 2.1, 1.0); tc: (1.931, 2.1, 0); norm: (0, -1, 0)),
  (pos: (1.931, 2.1, 1.531); tc: (1.931, 2.1, 1); norm: (0, -1, 0)),
  (pos: (3.131, 2.1, 1.0); tc: (3.131, 2.1, 0); norm: (0, -1, 0)),
  (pos: (3.131, 2.1, 1.531); tc: (3.131, 2.1, 1); norm: (0, -1, 0)),

  (pos: (0, 0, 0); tc: (0, 0, 1); norm: (0, -1, 0)),
  (pos: (0, 0, 1.0); tc: (1.0, 0, 1); norm: (0, -1, 0)),
  (pos: (0.531, 0, 0); tc: (0, 0, 0); norm: (0, -1, 0)),
  (pos: (0.531, 0, 1.0); tc: (1.0, 0, 0); norm: (0, -1, 0)),

  (pos: (0.531, 0, 1.531); tc: (0.531, 0, 1); norm: (0, -1, 0)),
  (pos: (4.531, 0, 1.531); tc: (4.531, 0, 1); norm: (0, -1, 0)),
  (pos: (0.531, 0, 1.0); tc: (0.531, 0, 0); norm: (0, -1, 0)) ,
  (pos: (4.531, 0, 1.0); tc: (4.531, 0, 0); norm: (0, -1, 0)),

  (pos: (0, 3, 0); tc: (0, 3, 1); norm: (0, 1, 0)),
  (pos: (0, 3, 1.0); tc: (1.0, 3, 1); norm: (0, 1, 0)),
  (pos: (0.531, 3, 0); tc: (0, 3, 0); norm: (0, 1, 0)),
  (pos: (0.531, 3, 1.0); tc: (1.0, 3, 0); norm: (0, 1, 0)),

  (pos: (0.531, 3, 1.531); tc: (0.531, 3, 1); norm: (0, 1, 0)),
  (pos: (4.531, 3, 1.531); tc: (4.531, 3, 1); norm: (0, 1, 0)),
  (pos: (0.531, 3, 1.0); tc: (0.531, 3, 0); norm: (0, 1, 0)) ,
  (pos: (4.531, 3, 1.0); tc: (4.531, 3, 0); norm: (0, 1, 0)),

  (pos: (0, 0, 1.0); tc: (0, 0, 1); norm: (0, -1, 0)),
  (pos: (0, 0, 1.531); tc: (0, 0, 1); norm: (0, -1, 0)),
  (pos: (0.531, 0, 1.0); tc: (0.531, 0, 0); norm: (0, -1, 0)),
  (pos: (0.531, 0, 1.531); tc: (0.531, 0, 1); norm: (0, -1, 0)),

  (pos: (0, 3, 1.0); tc: (0, 3, 1); norm: (0, 1, 0)),
  (pos: (0, 3, 1.531); tc: (0, 3, 1); norm: (0, 1, 0)),
  (pos: (0.531, 3, 1.0); tc: (0.531, 3, 0); norm: (0, 1, 0)),
  (pos: (0.531, 3, 1.531); tc: (0.531, 3, 1); norm: (0, 1, 0))
  );
implementation

end.
