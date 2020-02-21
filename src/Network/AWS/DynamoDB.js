'use strict';

const AWS = require('../Network.AWS').aws();

exports.documentClient = params => {
    return () => {
        return new AWS.DynamoDB.DocumentClient(params);
    };
};
