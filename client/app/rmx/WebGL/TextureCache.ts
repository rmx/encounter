/// <reference path="../../ext/three.d.ts" />

module rmx.WebGL {

    export class TextureCache {
        entries : Map<string, THREE.Texture> = new Map<string, THREE.Texture>();
    }


    function
    textureCacheKey(blobId: string): string {
        return blobId;
    }


    // lookupTexture
    // -----------------------------------------------------------------
    //
    // Return a texture for the given BlobId. The texture is cached so that
    // next time you request a texture with the same BlobId, the cached
    // texture will be returned.

    export function
    lookupTexture(cache: TextureCache, blobId: string): THREE.Texture {
        var key     = textureCacheKey(blobId)
          , texture = cache.entries.get(key);

        if (!texture) {
            texture = createTexture(blobId);
            cache.entries.set(key, texture);
        }

        return texture;
    }


    // createTexture
    // --------------------------------------------------------
    //
    // Create a new texture. The loading happens in the background.
    // But you can immediately use the texture in the scene, you don't
    // have to wait for it to load.

    function
    createTexture(blobId: string): THREE.Texture {
        var texture = new THREE.Texture(null)
          , blobUrl = rmx.blobUrl(blobId);

        loadImage(blobUrl).then(onResolve, onReject);

        function onResolve(img: HTMLImageElement) {
            texture.image       = toPowerOfTwoImage(img);
            texture.needsUpdate = true;
        }

        function onReject(ev) {
            console.warn("Failed to load texture", blobId, ":", ev);

            // Can we generate some fallback texture to indicate the failure,
            // so it's visible in the scene?
        }

        return texture;
    }


    // loadImage
    // ---------------------------------------------------------
    //
    // Load an image in the background. The promise will be resolved
    // with the HTMLImageElement if successful, or rejected with
    // the error event.

    function
    loadImage(url: string): Promise<HTMLImageElement> {
        return new Promise(function(resolve, reject) {
            var image = document.createElement('img');
            image.crossOrigin = 'anonymous';

            image.addEventListener('load', function(ev) {
                resolve(image);
            }, false);

            image.addEventListener('error', function(ev) {
                reject(ev);
            }, false);

            image.src = url;
        });
    }


    // TODO: Move to rmx.Math
    function isPowerOfTwo(x: number): boolean {
        return (x & (x - 1)) === 0 && x !== 0;
    }

    // TODO: Move to rmx.Math
    function nearestPowerOfTwo(x: number): number {
        var l = Math.log(x) / Math.LN2;
        return Math.pow(2, Math.round(l));
    }


    // toPowerOfTwoImage
    // ----------------------------------------------------------------
    //
    // Returns either a HTMLImageElement if the input is already
    // power-of-two, or else returns a HTMLCanvasElement which has
    // power-of-two dimensions.

    function toPowerOfTwoImage(img: HTMLImageElement): HTMLElement {
        if (!isPowerOfTwo(img.width) || !isPowerOfTwo(img.height)) {
            var w = nearestPowerOfTwo(img.width)
              , h = nearestPowerOfTwo(img.height);

            var canvas    = document.createElement('canvas');
            canvas.width  = w;
            canvas.height = h;

            var context   = canvas.getContext('2d');
            context.drawImage(img, 0, 0, w, h);

            return canvas;

        } else {
            return img;
        }
    }
}
