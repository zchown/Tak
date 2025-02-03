import * as BABYLON from "@babylonjs/core";
import * as GUI from "@babylonjs/gui";
import axios from "axios";

const canvas = document.getElementById("renderCanvas");
const engine = new BABYLON.Engine(canvas, true);
let boardState = null;
let currentPieces = new Map();

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

const createCells = (scene, boardState) => {
    const boardSize = boardState.board.length;
    const cellSize = 1;
    const cells = [];

    for (let y = 0; y < boardSize; y++) {
        for (let x = 0; x < boardSize; x++) {
            const cell = BABYLON.MeshBuilder.CreateBox(
                `cell-${x}-${y}`,
                { width: cellSize, height: 0.1, depth: cellSize },
                scene
            );
            cell.position.z = x * cellSize - (boardSize * cellSize) / 2;
            cell.position.x = y * cellSize - (boardSize * cellSize) / 2;
            cell.position.y = 0;

            const textureSize = 1024;
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

            ctx.font = "bold 96px Arial";
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
            cellMaterial.specularColor = new BABYLON.Color3(0.5, 0.5, 0.5);
            cellMaterial.emissiveColor = new BABYLON.Color3(0.5, 0.5, 0.5);
            cell.material = cellMaterial;

            cells.push(cell);
        }
    }

    return cells;
};

const createPieceKey = (x, y, index, piece) => {
    return `${x}-${y}-${index}-${piece.color}-${piece.type}`;
}

const updatePieces = (scene, newBoardState, cells) => {
    const boardSize = newBoardState.board.length;
    const cellSize = 1;
    const pieceScale = 0.5;
    const newPieces = new Map();

    const neededKeys = new Set();
    for (let y = 0; y < boardSize; y++) {
        for (let x = 0; x < boardSize; x++) {
            const stack = newBoardState.board[y][x];
            stack.forEach((piece, index) => {
                neededKeys.add(createPieceKey(x, y, index, piece));
            });
        }
    }

    currentPieces.forEach((mesh, key) => {
        if (!neededKeys.has(key)) {
            console.log("Disposing mesh:", key);
            mesh.dispose();
            currentPieces.delete(key);
        }
    });

    const newKeys = new Set([...neededKeys].filter(x => !currentPieces.has(x)));

    for (const p of newKeys) {
        let [x, y, index, color, type] = p.split("-");
        x = parseInt(x, 10);
        y = parseInt(y, 10);
        let pieceMesh;
        switch (type) {
            case "Flat":
                pieceMesh = BABYLON.MeshBuilder.CreateBox(
                    `piece-${p}`,
                    {
                        width: cellSize * pieceScale,
                        height: 0.1,
                        depth: cellSize * pieceScale
                    },
                    scene
                );
                pieceMesh.position.y = 0.1 + index * 0.15;
                break;
            case "Standing":
                pieceMesh = BABYLON.MeshBuilder.CreateBox(
                    `piece-${p}`,
                    {
                        width: 0.1,
                        height: cellSize * pieceScale,
                        depth: cellSize * pieceScale
                    },
                    scene
                );
                pieceMesh.rotation.y = Math.PI / 4;
                pieceMesh.position.y = 0.25 + index * 0.15;
                break;
            case "Cap":
                pieceMesh = BABYLON.MeshBuilder.CreateCylinder(
                    `piece-${p}`,
                    {
                        height: cellSize * pieceScale * 0.75,
                        diameter: cellSize * pieceScale * 0.75
                    },
                    scene
                );
                pieceMesh.position.y = 0.2 + index * 0.15;
                break;
        }
        pieceMesh.position.x = cells[y * boardSize + x].position.x;
        pieceMesh.position.z = cells[y * boardSize + x].position.z;
        pieceMesh.renderOutline = true;
        pieceMesh.outlineColor = BABYLON.Color3.Black();
        pieceMesh.outlineWidth = 0.01;
        
        const pieceMaterial = new BABYLON.StandardMaterial("piece-material", scene);
        const pieceColor = color === "White" ? 
            BABYLON.Color3.FromHexString(COLORS.lightPiece) : 
            BABYLON.Color3.FromHexString(COLORS.darkPiece);
        pieceMaterial.diffuseColor = pieceColor;
        pieceMaterial.specularColor = new BABYLON.Color3(0.5, 0.5, 0.5);
        pieceMaterial.emissiveColor = new BABYLON.Color3(0.1, 0.1, 0.1);
        pieceMesh.material = pieceMaterial;

        currentPieces.set(p, pieceMesh);
    };

};

const createGameStatePanel = (scene, gameState) => {
    const advancedTexture = GUI.AdvancedDynamicTexture.CreateFullscreenUI("UI");

    const mainContainer = new GUI.StackPanel();
    mainContainer.width = "650px";
    mainContainer.horizontalAlignment = GUI.Control.HORIZONTAL_ALIGNMENT_LEFT;
    mainContainer.verticalAlignment = GUI.Control.VERTICAL_ALIGNMENT_TOP;
    mainContainer.paddingLeft = "20px";
    advancedTexture.addControl(mainContainer);

    const gameStatePanel = new GUI.StackPanel();
    gameStatePanel.width = "100%";
    gameStatePanel.height = "225px";
    gameStatePanel.paddingTop = "20px";
    gameStatePanel.background = "rgba(0, 0, 0, 0.7)";
    mainContainer.addControl(gameStatePanel);

    const currentPlayerLabel = new GUI.TextBlock();
    currentPlayerLabel.text = `Current Player: ${gameState.currentPlayer}`;
    currentPlayerLabel.color = "white";
    currentPlayerLabel.fontSize = "36px";
    currentPlayerLabel.height = "50px";
    currentPlayerLabel.textHorizontalAlignment = GUI.Control.HORIZONTAL_ALIGNMENT_LEFT;
    gameStatePanel.addControl(currentPlayerLabel);

    const moveNumberLabel = new GUI.TextBlock();
    moveNumberLabel.text = `Move Number: ${gameState.moveNumber}`;
    moveNumberLabel.color = "white";
    moveNumberLabel.fontSize = "36px";
    moveNumberLabel.height = "50px";
    moveNumberLabel.textHorizontalAlignment = GUI.Control.HORIZONTAL_ALIGNMENT_LEFT;
    gameStatePanel.addControl(moveNumberLabel);

    const whiteReservesLabel = new GUI.TextBlock();
    const wrt = "Stones: " + gameState.whiteReserves.stones + " Caps: " + gameState.whiteReserves.caps;
    whiteReservesLabel.text = "White Reserves: " + wrt;
    whiteReservesLabel.color = "white";
    whiteReservesLabel.fontSize = "36px";
    whiteReservesLabel.height = "50px";
    whiteReservesLabel.textHorizontalAlignment = GUI.Control.HORIZONTAL_ALIGNMENT_LEFT;
    gameStatePanel.addControl(whiteReservesLabel);

    const blackReservesLabel = new GUI.TextBlock();
    const brt = "Stones: " + gameState.blackReserves.stones + " Caps: " + gameState.blackReserves.caps;
    blackReservesLabel.text = "Black Reserves: " + brt;
    blackReservesLabel.color = "white";
    blackReservesLabel.fontSize = "36px";
    blackReservesLabel.height = "50px";
    blackReservesLabel.textHorizontalAlignment = GUI.Control.HORIZONTAL_ALIGNMENT_LEFT;
    gameStatePanel.addControl(blackReservesLabel);

    return {
        panel: mainContainer,
        currentPlayerLabel,
        moveNumberLabel,
        whiteReservesLabel,
        blackReservesLabel,
    };
};

const updateBoard = (scene, newBoardState, cells) => {
    const existingPieces = scene.meshes.filter(mesh => 
        mesh.name.startsWith('piece-')
    );

    return updatePieces(scene, newBoardState, cells);
};

const addCellInteractivity = (scene, board) => {
    board.forEach((cell) => {
        cell.actionManager = new BABYLON.ActionManager(scene);
        cell.actionManager.registerAction(
            new BABYLON.ExecuteCodeAction(
                BABYLON.ActionManager.OnPickTrigger,
                (evt) => {
                    console.log("Cell clicked:", cell.name);
                    cell.material.diffuseColor = BABYLON.Color3.FromHexString("#18255b");
                }
            )
        );
    });
};

const createMoveInput = (scene, advancedTexture, gameId, cells, pieces) => {
    const inputContainer = new GUI.StackPanel();
    inputContainer.width = "650px";
    inputContainer.height = "200px";
    inputContainer.horizontalAlignment = GUI.Control.HORIZONTAL_ALIGNMENT_RIGHT;
    inputContainer.verticalAlignment = GUI.Control.VERTICAL_ALIGNMENT_TOP;
    inputContainer.paddingRight = "20px";
    inputContainer.paddingTop = "20px";
    inputContainer.background = "rgba(0, 0, 0, 0.0)";
    advancedTexture.addControl(inputContainer);

    const inputBox = new GUI.InputText();
    inputBox.width = "500px";
    inputBox.height = "100px";
    inputBox.color = "white";
    inputBox.background = "black";
    inputBox.placeholderText = "Enter move (e.g., a1, Sa1, Ca1)";
    inputBox.placeholderColor = "gray";
    inputBox.fontSize = "24px";
    inputBox.paddingTop = "20px";
    inputContainer.addControl(inputBox);

    const messageText = new GUI.TextBlock();
    messageText.height = "30px";
    messageText.color = "white";
    messageText.fontSize = "18px";
    inputContainer.addControl(messageText);

    const submitButton = GUI.Button.CreateSimpleButton("submitMove", "Submit Move");
    submitButton.width = "150px";
    submitButton.height = "40px";
    submitButton.color = "white";
    submitButton.background = "#567c8d";
    submitButton.fontSize = "20px";
    submitButton.cornerRadius = 5;
    submitButton.thickness = 2;
    inputContainer.addControl(submitButton);

    const submitMove = async () => {
        const moveNotation = inputBox.text.trim();
        if (!moveNotation) {
            messageText.text = "Please enter a move";
            messageText.color = "red";
            return;
        }

        try {
            console.log("Sending move request:", { gameId, moveNotation });
            const response = await axios.post("http://localhost:3000/api/game/move", {
                gameId,
                moveNotation
            });
            console.log("Move response:", response.data);

            if (response.data.responseStatus === "Success") {
                messageText.text = "Move successful!";
                messageText.color = "green";
                inputBox.text = "";

                const newBoardState = parseTPS(response.data.board);
                updateBoard(scene, newBoardState, cells);
                // updateGameStatePanel({
                
            } else {
                messageText.text = response.data.message;
                messageText.color = "red";
            }
        } catch (error) {
            messageText.text = "Error submitting move: " + (error.response?.data?.message || error.message);
            messageText.color = "red";
        }
    };

    submitButton.onPointerUpObservable.add(submitMove);
    inputBox.onKeyboardEventProcessedObservable.add((eventData) => {
        if (eventData.key === "Enter") {
            submitMove();
        }
    });

    return {
        container: inputContainer,
        inputBox,
        messageText,
        submitButton
    };
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
    const cells = createCells(scene, parsedBoard);
    addCellInteractivity(scene, cells);
    let pieces = updatePieces(scene, parsedBoard, cells);

    const { currentPlayerLabel, moveNumberLabel, whiteReservesLabel, blackReservesLabel } =
        createGameStatePanel(scene, {
            currentPlayer: gameState.currentPlayer,
            moveNumber: gameState.moveNum,
            whiteReserves: gameState.whiteReserves,
            blackReserves: gameState.blackReserves,
        });

    const moveInput = createMoveInput(scene, GUI.AdvancedDynamicTexture.CreateFullscreenUI("UI"), gameState.gameID, cells, pieces);

    const camera = new BABYLON.ArcRotateCamera(
        "camera",
        0,
        Math.PI / 4,
        7.5,
        new BABYLON.Vector3(0, 0, 0),
        scene
    );
    camera.attachControl(canvas, true);

    const hemisphericLight = new BABYLON.HemisphericLight(
        "hemisphericLight",
        new BABYLON.Vector3(0, 1, 1),
        scene
    );
    hemisphericLight.intensity = 0.8;

    engine.setHardwareScalingLevel(.5)

    const ppp = new BABYLON.PassPostProcess("pass", 1, camera)
    ppp.samples = engine.getCaps().maxMSAASamples

    const pipeline = new BABYLON.DefaultRenderingPipeline("defaultPipeline", true, scene, [camera])
    pipeline.samples = 4
    pipeline.fxaaEnabled = true
    pipeline.imageProcessingEnabled = false

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
