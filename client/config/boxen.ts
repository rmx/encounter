/// <reference path="./base.ts" />

module rmx {
    export module config {
        export var apiHost = 'http://api.rmx.dev';
        export var wsHost  = 'ws://localhost:3020';

        export var tutorialEncounterId = 'iaxYch0VMN';
    }

    export function apiUrl(path: string) {
        return config.apiHost + path;
    }

    export function blobUrl(blobId: string): string {
        return config.apiHost + '/blobs/' + blobId;
    }
}
