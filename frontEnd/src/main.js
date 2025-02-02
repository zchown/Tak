import * as BABYLON from "@babylonjs/core";
import axios from "axios";

const canvas = document.getElementById("renderCanvas");

const engine = new BABYLON.Engine(canvas, true);

const fetchGameState = async () => {
  try {
    const response = await axios.post("http://localhost:3000/api/game/new", {
      boardSize: 5,
    });
    return response.data;
  } catch (error) {
    console.error("Error fetching game state:", error);
    return null;
  }
};

const parseTPS = (tps) => {
  const [boardPart, player, moveNumber] = tps.split(" ");
  const rows = boardPart.split("/");
  const board = rows.map((row) => {
    const cells = [];
    let i = 0;
    while (i < row.length) {
      if (row[i] === "x") {
        const count = parseInt(row[i + 1], 10);
        for (let j = 0; j < count; j++) {
          cells.push(null);
        }
        i += 2;
      } else {
        cells.push(row[i]);
        i += 1;
      }
    }
    return cells;
  });
  return {
    board,
    currentPlayer: player === "1" ? "White" : "Black",
    moveNumber: parseInt(moveNumber, 10),
  };
};

const createBoard = (scene, boardState) => {
  const boardSize = boardState.board.length;
  const cellSize = 1; 
  const board = [];

  const color1 = new BABYLON.Color3(0.8, 0.7, 0.6); 
  const color2 = new BABYLON.Color3(0.5, 0.4, 0.3);

  for (let y = 0; y < boardSize; y++) {
    for (let x = 0; x < boardSize; x++) {
      const cell = BABYLON.MeshBuilder.CreateBox(
        `cell-${x}-${y}`,
        { width: cellSize, height: 0.1, depth: cellSize },
        scene
      );
      cell.position.x = x * cellSize - (boardSize * cellSize) / 2;
      cell.position.z = y * cellSize - (boardSize * cellSize) / 2;
      cell.position.y = 0;

      const cellMaterial = new BABYLON.StandardMaterial("cell-material", scene);

      if ((x + y) % 2 === 0) {
        cellMaterial.diffuseColor = color1; 
        cellMaterial.emissiveColor = color1;
      } else {
        cellMaterial.diffuseColor = color2; 
        cellMaterial.emissiveColor = color2;
      }

      cell.material = cellMaterial;

      const piece = boardState.board[y][x];
      if (piece) {
        const pieceMesh = BABYLON.MeshBuilder.CreateSphere(
          `piece-${x}-${y}`,
          { diameter: 0.8 },
          scene
        );
        pieceMesh.position.x = cell.position.x;
        pieceMesh.position.z = cell.position.z;
        pieceMesh.position.y = 0.6; 
        const pieceMaterial = new BABYLON.StandardMaterial("piece-material", scene);
        pieceMaterial.diffuseColor = piece === "W" ? BABYLON.Color3.White() : BABYLON.Color3.Black();
        pieceMesh.material = pieceMaterial;
      }

      board.push(cell);
    }
  }

  return board;
};

const addCellInteractivity = (scene, board) => {
  board.forEach((cell) => {
    cell.actionManager = new BABYLON.ActionManager(scene);
    cell.actionManager.registerAction(
      new BABYLON.ExecuteCodeAction(
        BABYLON.ActionManager.OnPickTrigger,
        (evt) => {
          console.log("Cell clicked:", cell.name);
          cell.material.diffuseColor = new BABYLON.Color3(1, 0, 0); 
        }
      )
    );
  });
};

const createScene = async () => {
  const scene = new BABYLON.Scene(engine);

  const gameState = await fetchGameState();
  console.log("Game State:", gameState);

  if (!gameState) {
    console.error("Failed to fetch game state");
    return scene;
  }

  const parsedBoard = parseTPS(gameState.board);

  const board = createBoard(scene, parsedBoard);

  addCellInteractivity(scene, board);

  const camera = new BABYLON.ArcRotateCamera(
    "camera",
    -Math.PI / 2,
    Math.PI / 2.5,
    10,
    new BABYLON.Vector3(0, 0, 0),
    scene
  );
  camera.attachControl(canvas, true);

  return scene;
};

const init = async () => {
  const scene = await createScene();

  engine.runRenderLoop(() => {
    scene.render();
  });

  window.addEventListener("resize", () => {
    engine.resize();
  });
};

init();
