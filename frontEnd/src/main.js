import * as BABYLON from "@babylonjs/core";
import axios from "axios";

const fetchGameState = async () => {
    try {
        const response = await axios.get("http://localhost:3000/api/game/some-game-id");
        return response.data;
    } catch (error) {
        console.error("Error fetching game state:", error);
        return null;
    }
};

const createScene = async () => {
    const scene = new BABYLON.Scene(engine);

    const gameState = await fetchGameState();
    console.log("Game State:", gameState);

    const camera = new BABYLON.ArcRotateCamera(
        "camera",
        -Math.PI / 2,
        Math.PI / 2.5,
        10,
        new BABYLON.Vector3(0, 0, 0),
        scene
    );
    camera.attachControl(canvas, true);

    const light = new BABYLON.HemisphericLight(
        "light",
        new BABYLON.Vector3(1, 1, 0),
        scene
    );

    const ground = BABYLON.MeshBuilder.CreateGround(
        "ground",
        { width: 6, height: 6 },

    );

    const sphere = BABYLON.MeshBuilder.CreateSphere(
        "sphere",
        { diameter: 2 },
        scene
    );
    sphere.position.y = 1;

    return scene;
};

createScene().then((scene) => {
    engine.runRenderLoop(() => {
        scene.render();
    });

    window.addEventListener("resize", () => {
        engine.resize();
    });
});
