/// <reference path="../Storage/Types.ts" />

/// <reference path="./TextureCache.ts" />
/// <reference path="./Scene.ts" />

module rmx.WebGL {

    // Mesh
    // -----------------------------------------------------------------------
    //
    // This class acts as a template from which the actual meshes are
    // instantiated. It loads the model's mesh in the background, and once the
    // mesh is available, you can create new instances with
    // 'createMeshInstance'.
    //
    // If we have any GL resources which can be shared between mesh instances,
    // this is where we'd keep the references to them.

    export class Mesh {

        mesh : THREE.Mesh = null;
        // ^ Initially null, once the data is loaded this will hold
        // a reference to the THREE Mesh. Use 'createMeshInstance' to get
        // a new copy of the mesh.

        constructor
          ( public model : rmx.Storage.Model
            // ^ The model which defines the mesh geometry and skins.
          ) {
        }
    }


    // createMesh
    // -----------------------------------------------------------------------

    export function
    createMesh(scene: Scene, model: rmx.Storage.Model): Mesh {
        var ret = new Mesh(model)
          , url = rmx.blobUrl(model.files[0].blobId);

        request.get(url).withCredentials().end((res: any) => {
            if (res.text) {
                try {
                    resolveWith(scene, ret, JSON.parse(res.text));
                } catch (e) {
                    console.warn('Error while resolving a mesh', e);
                }
            } else {
                console.warn("Failed to load model files", res);
            }
        });

        return ret;
    }


    // createMeshInstance
    // -----------------------------------------------------------------------
    //
    // Create a new instance of the given mesh, or nothing if the mesh has not
    // been loaded yet.

    export function
    createMeshInstance(mm: Mesh): THREE.Mesh {
        if (mm.mesh) {
            var mesh = mm.mesh.clone();
            (<any>mesh).customDepthMaterial = (<any>mm.mesh).customDepthMaterial;

            // FIXME: Only recompute normals if they are not already present.
            mesh.geometry.computeVertexNormals();

            mesh.castShadow    = true;
            mesh.receiveShadow = true;

            return mesh;
        }
    }


    // resolveWith
    // -----------------------------------------------------------------------

    function
    resolveWith(scene: Scene, mesh: Mesh, body: any): void {
        textureLoader.scene = scene;
        textureLoader.model = mesh.model;

        var threejsLoader = new THREE.JSONLoader(false)
          , result        = threejsLoader.parse(body, "/")
          , geometry      = result.geometry
          , material      = toMaterial(result.materials);

        var useSkinning = geometry.bones && geometry.bones.length > 0 && geometry.animation && geometry.animation.length > 0;
        var useMorph    = geometry.morphTargets && geometry.morphTargets.length > 0;

        // The three.js shader material behaves strangely, it expects a separate specular texture.
        // If none is present, tell three.js to use the diffuse texture, otherwise the material is way too bright.
        if (material.uniforms !== undefined) {
            var mu = material.uniforms;
            if (mu.tSpecular !== undefined && mu.tSpecular.value === null) {
                mu.enableSpecular.value = mu.enableDiffuse.value;
                mu.tSpecular.value      = mu.tDiffuse.value;
            }
        }

        // Disable alpha blending
        // Enable alpha masking (discard fragments with less than 95% opacity)
        forEachMaterial(material, function(m) {
            m.transparent = false;
            m.alphaTest = 0.05;
        });

        if (useSkinning) {
            mesh.mesh = new THREE.SkinnedMesh(geometry, material);
            forEachMaterial(material, function(m) {
                m.skinning = true;
            });

            var depthShader       = THREE.ShaderLib["depthRGBA"];
            var depthUniforms     = THREE.UniformsUtils.clone(depthShader.uniforms);
            var depthMaterialSkin = new THREE.ShaderMaterial({
                fragmentShader: depthShader.fragmentShader,
                vertexShader: depthShader.vertexShader,
                uniforms: depthUniforms,
                skinning: true
            });

            (<any>depthMaterialSkin)._shadowPass = true;
            (<any>mesh.mesh).customDepthMaterial = depthMaterialSkin;

        } else if (useMorph) {
            mesh.mesh = new THREE.MorphAnimMesh(geometry, material);
            forEachMaterial(material, function(m) {
                m.morphTargets = true;
                m.morphNormals = true;
            });
            geometry.computeMorphNormals();

        } else {
            mesh.mesh = new THREE.Mesh(geometry, material);
        }
    }


    function forEachMaterial(material: any, fn: Function): void {
        if (material instanceof THREE.MeshFaceMaterial) {
            material.materials.forEach(fn);
        } else {
            fn(material);
        }
    }


    function toMaterial(materials: any[]): any {
        if (materials && materials.length === 1) {
            return materials[0];
        } else if (materials && materials.length > 1) {
            return new THREE.MeshFaceMaterial(materials);
        } else {
            return new THREE.MeshBasicMaterial();
        }
    }



    // Ugly hack to enable texture cache while loading models.

    class TextureLoader {

        scene : Scene;
        model : rmx.Storage.Model;

        load(textureName0: string): THREE.Texture {
            var textureName = textureName0.substring(1, textureName0.length)
              , blobId      = textureBlobId(this.model, textureName);

            if (blobId) {
                return lookupTexture(this.scene.textureCache, blobId);
            } else {
                return new THREE.Texture(null);
            }
        }
    }

    var textureLoader = new TextureLoader;
    (<any>THREE.Loader).Handlers.add(/.*/, textureLoader);


    function textureBlobId(model, textureName): string {
        var skinName = 'default';

        for (var i = 0; i < model.skins.length; ++i) {
            if (skinName === model.skins[i].name) {
                for (var j = 0; j < model.skins[i].textures.length; ++j) {
                    if (textureName === model.skins[i].textures[j].type) {
                        return model.skins[i].textures[j].blobId;
                    }
                }
            }
        }
    }
}
