import * as BABYLON from "@babylonjs/core";
import * as GUI from "@babylonjs/gui";
import axios from "axios";

const canvas = document.getElementById("renderCanvas");
const engine = new BABYLON.Engine(canvas, true);
let currentPieces = new Map();
let gameStatePanel = null;
let gameHistoryPanel = null;
let moveInput = null;

const COLORS = {
    lightSquare: '#c8d9e6',
    darkSquare: '#567c8d',
    lightPiece: '#f5efeb',
    darkPiece: '#2f4156'
};

const fetchGameState = async () => {
    try {
        const response = await axios.post("http://localhost:3000/api/game/new", {
            boardSize: 6,
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

            ctx.font = "bold 160px Arial";
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
            cellMaterial.specularColor = new BABYLON.Color3(0.3, 0.3, 0.3);
            cellMaterial.emissiveColor = new BABYLON.Color3(1, 1, 1);
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
            case "Standing": 
                if (color === "White") {
                    pieceMesh = BABYLON.MeshBuilder.CreateBox(
                        `piece-${p}`,
                        {
                            width: cellSize * pieceScale,
                            height: 0.1,
                            depth: cellSize * pieceScale
                        },
                        scene
                    );
                    const vertices = pieceMesh.getVerticesData(BABYLON.VertexBuffer.PositionKind);
                    const newVertices = [];
                    for (let i = 0; i < vertices.length; i += 3) {
                        const x = vertices[i];
                        const y = vertices[i + 1];
                        const z = vertices[i + 2];
                        if (z > 0) {
                            newVertices.push(x * 0.8, y, z * 0.8);
                        } else {
                            newVertices.push(x, y, z);
                        }
                    }
                    pieceMesh.setVerticesData(BABYLON.VertexBuffer.PositionKind, newVertices);
                } else {
                    const radius = (cellSize * pieceScale) / 2;
                    const segmentAngle = Math.PI / 2; 
                    const totalAngle = 2 * Math.PI; 
                    const largerAngle = totalAngle - segmentAngle; 
                    const segmentHeight = 0.12;

                    pieceMesh = new BABYLON.Mesh(`piece-${p}`, scene);
                    const positions = [];
                    const indices = [];

                    for (let i = 0; i <= 24; i++) {
                        const theta = (segmentAngle / 2) + (i / 24) * largerAngle;
                        const xPos = radius * Math.cos(theta);
                        const zPos = radius * Math.sin(theta);
                        positions.push(xPos, -0.05, zPos);
                    }

                    for (let i = 0; i <= 24; i++) {
                        const theta = (segmentAngle / 2) + (i / 24) * largerAngle;
                        const xPos = radius * Math.cos(theta);
                        const zPos = radius * Math.sin(theta);
                        positions.push(xPos, segmentHeight -0.05, zPos);
                    }

                    for (let i = 0; i < 24; i++) {
                        indices.push(i, i + 1, 25 + i);
                        indices.push(i + 1, 25 + i + 1, 25 + i);
                    }

                    for (let i = 1; i < 24; i++) {
                        indices.push(0, i, i + 1);
                    }

                    for (let i = 1; i < 24; i++) {
                        indices.push(25, 25 + i, 25 + i + 1);
                    }

                    const startBottom = 0;
                    const endBottom = 24;
                    const startTop = 25; 
                    const endTop = 49; 

                    indices.push(endBottom, startBottom, startTop);
                    indices.push(endBottom, startTop, endTop);

                    pieceMesh.setVerticesData(BABYLON.VertexBuffer.PositionKind, positions);
                    pieceMesh.setIndices(indices);
                }

                if (type === "Standing") {
                    if (color == "White") {
                        pieceMesh.rotation.x = - Math.PI / 2; 
                        pieceMesh.rotation.y = - Math.PI / 4;
                    }
                    else {
                        pieceMesh.rotation.z = - Math.PI / 2;
                        pieceMesh.rotation.y = Math.PI / 4;
                    }
                    pieceMesh.position.y = 0.25 + index * 0.12; 
                } else {
                    pieceMesh.position.y = 0.1 + index * 0.12; 
                    pieceMesh.rotation.y = Math.random() * Math.PI * 2;
                }
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
                pieceMesh.position.y = 0.25 + index * 0.12;
                break;
        }
        pieceMesh.position.x = cells[y * boardSize + x].position.x;
        pieceMesh.position.z = cells[y * boardSize + x].position.z;

        pieceMesh.convertToFlatShadedMesh();
        pieceMesh.enableEdgesRendering();
        pieceMesh.edgesWidth = 2.0;
        if (color == "Black") { 
            pieceMesh.edgesColor = new BABYLON.Color4(0.8, 0.8, 0.8, 0.8); 
        } else {
            pieceMesh.edgesColor = new BABYLON.Color4(0.2, 0.2, 0.2, 0.8);
        }

        const pieceMaterial = new BABYLON.StandardMaterial("piece-material", scene);
        const pieceColor = color === "White" ? 
            BABYLON.Color3.FromHexString(COLORS.lightPiece) : 
            BABYLON.Color3.FromHexString(COLORS.darkPiece);
        pieceMaterial.diffuseColor = pieceColor;
        pieceMaterial.specularColor = new BABYLON.Color3(0.3, 0.3, 0.3);
        pieceMaterial.backFaceCulling = false;
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

    gameStatePanel = new GUI.StackPanel();
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

    const whiteReservesLabel = new GUI.TextBlock();
    whiteReservesLabel.text = `White Reserves: Stones: ${gameState.whiteReserves.stones} Caps: ${gameState.whiteReserves.caps}`;
    whiteReservesLabel.color = "white";
    whiteReservesLabel.fontSize = "36px";
    whiteReservesLabel.height = "50px";
    whiteReservesLabel.textHorizontalAlignment = GUI.Control.HORIZONTAL_ALIGNMENT_LEFT;
    gameStatePanel.addControl(whiteReservesLabel);

    const blackReservesLabel = new GUI.TextBlock();
    blackReservesLabel.text = `Black Reserves: Stones: ${gameState.blackReserves.stones} Caps: ${gameState.blackReserves.caps}`;
    blackReservesLabel.color = "white";
    blackReservesLabel.fontSize = "36px";
    blackReservesLabel.height = "50px";
    blackReservesLabel.textHorizontalAlignment = GUI.Control.HORIZONTAL_ALIGNMENT_LEFT;
    gameStatePanel.addControl(blackReservesLabel);

    gameHistoryPanel = new GUI.StackPanel();
    gameHistoryPanel.width = "100%";
    gameHistoryPanel.height = "300px";
    gameHistoryPanel.paddingTop = "10px";
    gameHistoryPanel.background = "rgba(0, 0, 0, 0.7)";
    mainContainer.addControl(gameHistoryPanel);

    const historyLabel = new GUI.TextBlock();
    historyLabel.text = "Game History";
    historyLabel.color = "white";
    historyLabel.fontSize = "50px";
    historyLabel.height = "50px";
    historyLabel.textHorizontalAlignment = GUI.Control.HORIZONTAL_ALIGNMENT_CENTER;
    gameHistoryPanel.addControl(historyLabel);

    const scrollViewer = new GUI.ScrollViewer();
    scrollViewer.width = "100%";
    scrollViewer.height = "250px";
    scrollViewer.thickness = 0;
    gameHistoryPanel.addControl(scrollViewer);

    const historyText = new GUI.TextBlock();
    historyText.text = gameHistoryToText(gameState);
    historyText.color = "white";
    historyText.fontSize = "36px";
    historyText.textWrapping = GUI.TextWrapping.WordWrap;
    historyText.resizeToFit = true;
    scrollViewer.addControl(historyText);

    return {
        panel: mainContainer,
        currentPlayerLabel,
        whiteReservesLabel,
        blackReservesLabel,
        historyText,
        scrollViewer,
    };
};

const updateGameStatePanel = (gameState) => {
    gameStatePanel.currentPlayerLabel.text = `Current Player: ${gameState.currentPlayer}`;
    console.log(gameState);
    const wrt = "Stones: " + gameState.whiteReserves.stones + " Caps: " + gameState.whiteReserves.caps;
    gameStatePanel.whiteReservesLabel.text = "White Reserves: " + wrt;
    const brt = "Stones: " + gameState.blackReserves.stones + " Caps: " + gameState.blackReserves.caps;
    gameStatePanel.blackReservesLabel.text = "Black Reserves: " + brt;
    gameStatePanel.historyText.text = gameHistoryToText(gameState);
}

const updateBoard = (scene, newBoardState, cells) => {
    const existingPieces = scene.meshes.filter(mesh => 
        mesh.name.startsWith('piece-')
    );

    return updatePieces(scene, newBoardState, cells);
};

const gameHistoryToText = (gameState) => {
    const history = gameState.gameHistory.reverse();
    let text = "";
    let moveNumber = 1;
    for (let i = 0; i < history.length; i++) {
        if (i % 2 == 0) {
            text += moveNumber + ". "
            moveNumber++;
        }

        text += history[i] 

        if (i % 2 == 1) {
            text += "\n";
        }
        else {
            text += "       ";
        }
    }
    return text;
};

const moveToAlgebraic = (move) => {

};

const createMoveInput = (scene, advancedTexture, gameId, cells, pieces) => {
    const inputContainer = new GUI.StackPanel();
    inputContainer.width = "650px";
    inputContainer.height = "250px";
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
    inputBox.autoFocus = true;
    inputContainer.addControl(inputBox);

    const messageText = new GUI.TextBlock();
    messageText.height = "50px";
    messageText.color = "white";
    messageText.fontSize = "28px";
    inputContainer.addControl(messageText);

    const submitButton = GUI.Button.CreateSimpleButton("submitMove", "Submit Move");
    submitButton.width = "150px";
    submitButton.height = "50px";
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
                updateGameStatePanel(response.data);
                moveInput.inputBox.focus();

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
    scene.clearColor = BABYLON.Color3.FromHexString("#1c2833");

    const gameState = await fetchGameState();
    console.log("Game State:", gameState);

    if (!gameState) {
        console.error("Failed to fetch game state");
        return scene;
    }

    const parsedBoard = parseTPS(gameState.board);
    const cells = createCells(scene, parsedBoard);
    let pieces = updatePieces(scene, parsedBoard, cells);

    gameStatePanel = createGameStatePanel(scene, gameState);

    moveInput = createMoveInput(scene, GUI.AdvancedDynamicTexture.CreateFullscreenUI("UI"), gameState.gameID, cells, pieces);
    moveInput.inputBox.focus();

    const camera = new BABYLON.ArcRotateCamera(
        "camera",
        0,
        Math.PI / 4,
        7.5,
        new BABYLON.Vector3(0, 0, -0.5),
        scene
    );
    camera.lowerRadiusLimit = 5;
    camera.upperRadiusLimit = 10;
    camera.attachControl(canvas, true);

    const hemisphericLight = new BABYLON.HemisphericLight(
        "hemisphericLight",
        new BABYLON.Vector3(-5, 3, 1), 
        scene
    );
    hemisphericLight.intensity = 0.7;

    const hemisphericLight2 = new BABYLON.HemisphericLight(
        "hemisphericLight2",
        new BABYLON.Vector3(-1, 3, -2), 
        scene
    );
    hemisphericLight2.intensity = 0.7;
    hemisphericLight2.diffuse = new BABYLON.Color3(0.5, 0.5, 0.5);

    engine.setHardwareScalingLevel(0.5);

    const ppp = new BABYLON.PassPostProcess("pass", 1, camera);
    ppp.samples = engine.getCaps().maxMSAASamples;

    const pipeline = new BABYLON.DefaultRenderingPipeline("defaultPipeline", true, scene, [camera]);
    pipeline.samples = 4;
    pipeline.fxaaEnabled = true;
    pipeline.imageProcessingEnabled = false;
    pipeline.bloomEnabled = true;
    pipeline.bloomThreshold = 0.7;
    pipeline.bloomWeight = 0.05;
    pipeline.bloomScale = 0.2;
    pipeline.anisotropicFilteringEnabled = true;
    pipeline.chromaticAberrationEnabled = true;
    pipeline.chromaticAberration.aberrationAmount = 0.1;
    pipeline.vignetteEnabled = true;
    pipeline.depthOfFieldEnabled = false;


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
