import * as BABYLON from "@babylonjs/core";
import axios from "axios";

const canvas = document.getElementById("renderCanvas");
const engine = new BABYLON.Engine(canvas, true);

const COLORS = {
    lightSquare: '#c8d9e6',
    darkSquare: '#567c8d',
    lightPiece: '#f5efeb',
    darkPiece: '#2f4156'
};

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
    const [boardPart, turnStr, moveNumberStr] = tps.split(" ");
    const rows = boardPart.split("/");
    const board = rows.map((row) => {
        const cells = [];
        const squares = row.split(",");
        for (const square of squares) {
            if (square.startsWith("x")) {
                const count = parseInt(square.slice(1), 10) || 1;
                for (let i = 0; i < count; i++) {
                    cells.push([]);
                }
            } else {
                const stack = [];
                let i = 0;
                while (i < square.length) {
                    const color = square[i] === "1" ? "White" : "Black";
                    const type = square[i + 1] || "Flat";
                    switch (type) {
                        case "S":
                            stack.push({ color, type: "Standing" });
                            i += 2;
                            break;
                        case "C":
                            stack.push({ color, type: "Cap" });
                            i += 2;
                            break;
                        default:
                            stack.push({ color, type: "Flat" });
                            i += 1;
                            break;
                    }
                }
                cells.push(stack);
            }
        }
        return cells;
    });
    return {
        board,
        currentPlayer: turnStr === "1" ? "White" : "Black",
        moveNumber: parseInt(moveNumberStr, 10),
    };
};

const createBoard = (scene, boardState) => {
    const boardSize = boardState.board.length;
    const cellSize = 1;
    const pieceScale = 0.6;
    const board = [];

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

            const textureSize = 512;
            const dynamicTexture = new BABYLON.DynamicTexture(
                `cell-texture-${x}-${y}`,
                textureSize,
                scene,
                true
            );

            const ctx = dynamicTexture.getContext();
            const isLightSquare = (x + y) % 2 === 0;
            ctx.fillStyle = isLightSquare ? COLORS.lightSquare : COLORS.darkSquare;
            ctx.fillRect(0, 0, textureSize, textureSize);

            ctx.font = "bold 48px Arial";
            ctx.fillStyle = isLightSquare ? COLORS.darkSquare : COLORS.lightSquare;
            ctx.textAlign = "right";
            ctx.textBaseline = "bottom";

            const columnLabel = String.fromCharCode(97 + x);
            const rowLabel = (boardSize - y).toString();
            const labelText = `${columnLabel}${rowLabel}`;

            const padding = 20;
            ctx.fillText(labelText, textureSize - padding, textureSize - padding);
            dynamicTexture.update();

            const cellMaterial = new BABYLON.StandardMaterial(`cell-material-${x}-${y}`, scene);
            cellMaterial.diffuseTexture = dynamicTexture;
            cellMaterial.specularColor = new BABYLON.Color3(0.1, 0.1, 0.1);
            cellMaterial.emissiveColor = new BABYLON.Color3(0.1, 0.1, 0.1);
            cell.material = cellMaterial;

            const stack = boardState.board[y][x];
            if (stack.length > 0) {
                for (let i = 0; i < stack.length; i++) {
                    const piece = stack[i];
                    let pieceMesh;

                    switch (piece.type) {
                        case "Flat":
                            pieceMesh = BABYLON.MeshBuilder.CreateBox(
                                `piece-${x}-${y}-${i}`,
                                {
                                    width: cellSize * pieceScale,
                                    height: 0.1,
                                    depth: cellSize * pieceScale
                                },
                                scene
                            );
                            break;
                        case "Standing":
                            pieceMesh = BABYLON.MeshBuilder.CreateBox(
                                `piece-${x}-${y}-${i}`,
                                {
                                    width: 0.1,
                                    height: cellSize * pieceScale,
                                    depth: cellSize * pieceScale
                                },
                                scene
                            );
                            break;
                        case "Cap":
                            pieceMesh = BABYLON.MeshBuilder.CreateCylinder(
                                `piece-${x}-${y}-${i}`,
                                {
                                    height: cellSize * pieceScale,
                                    diameter: cellSize * pieceScale
                                },
                                scene
                            );
                            break;
                    }

                    pieceMesh.position.x = cell.position.x;
                    pieceMesh.position.z = cell.position.z;
                    pieceMesh.position.y = 0.1 + i * 0.15;

                    const pieceMaterial = new BABYLON.StandardMaterial("piece-material", scene);
                    const pieceColor = piece.color === "White" ? 
                        BABYLON.Color3.FromHexString(COLORS.lightPiece) : 
                        BABYLON.Color3.FromHexString(COLORS.darkPiece);
                    pieceMaterial.diffuseColor = pieceColor;
                    pieceMesh.material = pieceMaterial;
                }
            }

            board.push(cell);
        }
    }

    const hemisphericLight = new BABYLON.HemisphericLight(
        "hemisphericLight",
        new BABYLON.Vector3(0, 1, 0),
        scene
    );
    hemisphericLight.intensity = 0.7;

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

    const light = new BABYLON.DirectionalLight(
        "light",
        new BABYLON.Vector3(-1, -1, 0),
        scene
    );

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
