'use strict';

const AWS = require('../Network.AWS').aws();

exports._documentClient = params => {
    return () => {
        return new AWS.DynamoDB.DocumentClient(params);
    };
};
