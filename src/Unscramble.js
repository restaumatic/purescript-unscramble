function DecodingError(message) {
  this.message = message;
}

function decodingError(msg) {
  throw new DecodingError(msg);
}

exports.decodingError = decodingError;

// catchDecodingErrorImpl :: forall a r. (String -> r) -> (a -> r) -> (Partial => a) -> r
exports.catchDecodingErrorImpl = onError => onSuccess => fn => {
  try {
    return onSuccess(fn());
  } catch(e) {
    if(e instanceof DecodingError) {
      return onError(e.message);
    } else {
      throw e;
    }
  }
};

exports.unsafeParseJSON = json => {
  try {
    return JSON.parse(json);
  } catch(e) {
    if(e instanceof SyntaxError) {
      decodingError("Invalid JSON: " + e.message);
    } else {
      throw e;
    }
  }
};

exports.decodeString = x => {
  if(typeof x === 'string') {
    return x;
  } else {
    decodingError("Expected String");
  }
};

exports.decodeNumber = x => {
  if(typeof x === 'number') {
    return x;
  } else {
    decodingError("Expected Number");
  }
};

exports.decodeInt = x => {
  if(typeof x === 'number' && Math.floor(x) === x) {
    return x;
  } else {
    decodingError("Expected Int");
  }
};

exports.decodeBoolean = x => {
  if(typeof x === 'boolean') {
    return x;
  } else {
    decodingError("Expected Boolean");
  }
};

exports.decodeArray = decodeItem => x => {
  if(typeof x === 'object' && x instanceof Array) {
    // TODO: error reporting with index
    return x.map(decodeItem);
  } else {
    decodingError("Expected Array");
  }
};

exports.decodeObject = decodeItem => x => {
  if(typeof x === 'object' && !(x instanceof Array)) {
    const result = {};
    for(const key in x) {
      result[key] = decodeItem(x[key]);
    }
    return result;
  } else {
    decodingError("Expected Object");
  }
};
