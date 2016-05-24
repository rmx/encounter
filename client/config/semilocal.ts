/// <reference path="./base.ts" />

module rmx {
    export module config {
        export var apiHost = 'https://api.rmx.im';
        export var wsHost  = 'ws://localhost:3020';

        export var tutorialEncounterId = 'FzpEtMJC0m';
    }

    export function apiUrl(path: string) {
        return config.apiHost + path;
    }

    export function blobUrl(blobId: string): string {
        return 'https://blobs.rmx.im/' + blobId;
    }
}
