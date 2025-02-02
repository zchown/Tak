import * as BABYLON from "@babylonjs/core";
import * as GUI from "@babylonjs/gui";
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
    const board = [];

    const color1 = new BABYLON.Color3(0.8, 0.7, 0.6); 
    const color2 = new BABYLON.Color3(0.5, 0.4, 0.3); 

    const advancedTexture = GUI.AdvancedDynamicTexture.CreateFullscreenUI("UI");

    const createLabel = (text, x, z) => {
        const label = new GUI.TextBlock();
        label.text = text;
        label.color = "black";
        label.fontSize = 14;

        const anchor = BABYLON.MeshBuilder.CreateBox(
            `anchor-${text}`,
            { size: 0.01 },
            scene
        );
        anchor.position = new BABYLON.Vector3(
            x * cellSize - (boardSize * cellSize) / 2 + 0.1,
            0.15,
            z * cellSize - (boardSize * cellSize) / 2 + 0.1
        );
        anchor.visibility = 0;

        label.linkWithMesh(anchor);
        label.linkOffsetY = -50;

        advancedTexture.addControl(label);
        return label;
    };

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
            } else {
                cellMaterial.diffuseColor = color2; 
            }

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
                                { width: 0.8, height: 0.1, depth: 0.8 },
                                scene
                            );
                            break;
                        case "Standing":
                            pieceMesh = BABYLON.MeshBuilder.CreateBox(
                                `piece-${x}-${y}-${i}`,
                                { width: 0.1, height: 0.8, depth: 0.8 },
                                scene
                            );
                            break;
                        case "Cap":
                            pieceMesh = BABYLON.MeshBuilder.CreateCylinder(
                                `piece-${x}-${y}-${i}`,
                                { height: 0.8, diameter: 0.8 },
                                scene
                            );
                            break;
                    }
                    pieceMesh.position.x = cell.position.x;
                    pieceMesh.position.z = cell.position.z;
                    pieceMesh.position.y = 0.1 + i * 0.2; 
                    const pieceMaterial = new BABYLON.StandardMaterial("piece-material", scene);
                    pieceMaterial.diffuseColor = piece.color === "White" ? BABYLON.Color3.White() : BABYLON.Color3.Black();
                    pieceMesh.material = pieceMaterial;
                }
            }


            const columnLabel = String.fromCharCode(97 + x); 
            const rowLabel = (boardSize - y).toString(); 
            const labelText = `${columnLabel}${rowLabel}`;
            createLabel(labelText, x, y);

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
