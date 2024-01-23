import {
  GenericContainer,
  PullPolicy,
  Wait,
} from "testcontainers";

/**
 * @param {GenericContainer} container
 * @returns {GenericContainer}
 */
const cloneContainer = container => {
  const clone = new GenericContainer("");
  Object.assign(clone, container);

  return clone;
};

export const mkContainerImpl = Constructor => image => Constructor(new GenericContainer(image));

export const mkContainerFromDockerfileImpl = Constructor => contextPath => image => async () => {
  const container = await GenericContainer.fromDockerfile(contextPath).withCache(false).build(image);
  return Constructor(container);
};

export const mkContainerFromDockerfileCustomDockerfileImpl = Constructor => contextPath => dockerFile => image => async () => {
  const container = await GenericContainer.fromDockerfile(contextPath, dockerFile).withCache(false).build(image);
  return Constructor(container);
};

export const setPullPolicyImpl = GC => Constructor => PP => {
  const clone = cloneContainer(GC.value1);
  const imagePullPolicy = PP.constructor.name == "AlwaysPull" ? PullPolicy.alwaysPull() : PullPolicy.defaultPolicy();
  clone.withPullPolicy(imagePullPolicy);
  return Constructor(clone);
};

export const setCommandImpl = GC => Constructor => command => {
  const clone = cloneContainer(GC.value1);
  clone.withCommand(command);
  return Constructor(clone);
};

export const setAddedCapabilitiesImpl = GC => Constructor => capabilities => {
  const clone = cloneContainer(GC.value1);
  clone.withAddedCapabilities(...capabilities);
  return Constructor(clone);
};

export const setDroppedCapabilitiesImpl = GC => Constructor => capabilities => {
  const clone = cloneContainer(GC.value1);
  clone.withDroppedCapabilities(...capabilities);
  return Constructor(clone);
};

export const setEntrypointImpl = GC => Constructor => entryPoint => {
  const clone = cloneContainer(GC.value1);
  clone.withEntrypoint(entryPoint);
  return Constructor(clone);
};

export const setIpcModeImpl = GC => Constructor => ipcMode => {
  const clone = cloneContainer(GC.value1);
  clone.withIpcMode(ipcMode);
  return Constructor(clone);
};

export const setResourcesQuotaImpl = GC => Constructor => resourcesQuota => {
  const clone = cloneContainer(GC.value1);
  clone.withResourcesQuota(resourcesQuota);
  return Constructor(clone);
};

export const setSharedMemorySizeImpl = GC => Constructor => sharedMemorySize => {
  const clone = cloneContainer(GC.value1);
  clone.withSharedMemorySize(sharedMemorySize);
  return Constructor(clone);
};

export const setReuseImpl = GC => Constructor => {
  const clone = cloneContainer(GC.value1);
  clone.withReuse();
  return Constructor(clone);
};

export const setEnvironmentImpl = GC => Constructor => env => {
  const toEnv = env.reduce((acc, { key, value }) => {
    acc[key] = value;
    return acc;
  }, {});

  const clone = cloneContainer(GC.value1);
  clone.withEnvironment(toEnv);
  return Constructor(clone);
};

export const setLabelsImpl = GC => Constructor => labels => {
  const toLabels = labels.reduce((acc, { key, value }) => {
    acc[key] = value;
    return acc;
  }, {});

  const clone = cloneContainer(GC.value1);
  clone.withLabels(toLabels);
  return Constructor(clone);
};

export const setExposedPortsImpl = GC => Constructor => ports => {
  const clone = cloneContainer(GC.value1);
  clone.withExposedPorts(...ports);
  return Constructor(clone);
};

export const setBindMountsImpl = GC => Constructor => bindMounts => {
  const bm = bindMounts.map(({ source, target, readOnly }) => ({ source, target, mode: readOnly ? "ro" : "rw" }));
  const clone = cloneContainer(GC.value1);
  clone.withBindMounts(bm);
  return Constructor(clone);
};

export const setNameImpl = GC => Constructor => name => {
  const clone = cloneContainer(GC.value1);
  clone.withName(name);
  return Constructor(clone);
};

export const setCopyFilesToContainerImpl = GC => Constructor => copyFiles => {
  const clone = cloneContainer(GC.value1);

  copyFiles.forEach(cp => {
    if (cp.constructor.name == "FromSource") {
      clone.withCopyFilesToContainer([{ source: cp.value0, target: cp.value1, mode: parseInt(cp.value2, 8) }]);
    } else if (cp.constructor.name == "FromContent") {
      clone.withCopyContentToContainer([{ content: cp.value0, target: cp.value1, mode: parseInt(cp.value2, 8) }]);
    } else if (cp.constructor.name == "FromDirectory") {
      clone.withCopyDirectoriesToContainer([{ source: cp.value0, target: cp.value1, mode: parseInt(cp.value2, 8) }]);
    };
  });

  return Constructor(clone);
};

export const setTmpFsImpl = GC => Constructor => tmpfs => {
  const clone = cloneContainer(GC.value1);
  const tmpfsObj = {};
  tmpfsObj[tmpfs.path] = tmpfs.mountOptions;

  clone.withTmpFs(tmpfsObj);
  return Constructor(clone);
};

export const setUserImpl = GC => Constructor => user => {
  const clone = cloneContainer(GC.value1);
  clone.withUser(user);
  return Constructor(clone);
};

export const setPrivilegedModeImpl = GC => Constructor => {
  const clone = cloneContainer(GC.value1);
  clone.withPrivilegedMode();
  return Constructor(clone);
};

export const setWorkingDirectoryImpl = GC => Constructor => workingDir => {
  const clone = cloneContainer(GC.value1);
  clone.withWorkingDir(workingDir);
  return Constructor(clone);
};

export const setDefaultLogDriverImpl = GC => Constructor => {
  const clone = cloneContainer(GC.value1);
  clone.withDefaultLogDriver();
  return Constructor(clone);
};

export const setStartupTimeoutImpl = GC => Constructor => startupTimeout => {
  const clone = cloneContainer(GC.value1);
  clone.withStartupTimeout(startupTimeout);
  return Constructor(clone);
};

export const setWaitStrategyImpl = GC => Constructor => waitStrategies => {
  const clone = cloneContainer(GC.value1);

  const toJs = waitStrategies.map(w => {
    if (w.constructor.name == "ListeningPorts") {
      return Wait.forListeningPorts();
    } else if (w.constructor.name == "LogOutput") {
      return Wait.forLogMessage(w.value0, w.value1);
    } else if (w.constructor.name == "HealthCheck") {
      return Wait.forHealthCheck();
    } else if (w.constructor.name == "HttpStatusCode") {
      return Wait.forHttp(w.value0, w.value1).forStatusCode(w.value2);
    } else if (w.constructor.name == "HttpResponsePredicate") {
      return Wait.forHttp(w.value0, w.value1).forResponsePredicate(w.value2);
    } else if (w.constructor.name == "ShellCommand") {
      return Wait.forSuccessfulCommand(w.value0);
    };
  }).filter(x => x !== undefined);

  clone.withWaitStrategy(Wait.forAll(toJs));
  return Constructor(clone);
};

export const setNetworkModeImpl = GC => Constructor => networkMode => {
  const clone = cloneContainer(GC.value1);
  clone.withNetworkMode(networkMode);
  return Constructor(clone);
};

export const setExtraHostsImpl = GC => Constructor => extraHosts => {
  const clone = cloneContainer(GC.value1);
  clone.withExtraHosts(extraHosts);
  return Constructor(clone);
};

export const setNetworkAliasesImpl = GC => Constructor => networkAliases => {
  const clone = cloneContainer(GC.value1);
  clone.withNetworkAliases(...networkAliases);
  return Constructor(clone);
};

export const setNetworkImpl = GC => Constructor => network => {
  const clone = cloneContainer(GC.value1);
  clone.withNetwork(network.value0);
  return Constructor(clone);
};

export const getMappedPortImpl = GC => port => Left => Right => () => {
  try {
    return Right(GC.value1.getMappedPort(port));
  } catch (e) {
    return Left(e.message);
  }
};

export const getFirstMappedPortImpl = GC => Left => Right => () => {
  try {
    return Right(GC.value1.getFirstMappedPort());
  } catch (e) {
    return Left(e.message);
  }
};

export const execImpl = GC => cmds => Left => Right => async () => {
  try {
    return Right(await GC.value1.exec(cmds));
  } catch (e) {
    return Left(e.message);
  }
};

export const getNameImpl = GC => Left => Right => () => {
  try {
    return Right(GC.value1.getName());
  } catch (e) {
    return Left(e.message);
  }
};

export const getIdImpl = GC => Left => Right => () => {
  try {
    return Right(GC.value1.getId());
  } catch (e) {
    return Left(e.message);
  }
};

export const getHostImpl = GC => Left => Right => () => {
  try {
    return Right(GC.value1.getHost());
  } catch (e) {
    return Left(e.message);
  }
};

/**
  * @param {{value1: GenericContainer}} StoppedContainer
  */
export const startContainerImpl = StoppedContainer => StartedContainer => Right => Left => async () => {
  const container = StoppedContainer.value1;

  try {
    const res = await container.start();
    return Right(StartedContainer(res));
  } catch (e) {
    return Left(e.message);
  }
};

/**
 * @param {{value1: StartedTestContainer}} StartedContainer
 */
export const restartContainerImpl = StartedContainer => StartedContainerCons => Right => Left => async () => {
  const container = StartedContainer.value1;

  try {
    await container.restart();
    return Right(StartedContainerCons(container));
  } catch (e) {
    return Left(e.message);
  }
};

/**
 * @param {{value1: StartedTestContainer}} StartedContainer
 */
export const stopContainerImpl = StartedContainer => StoppedContainer => Right => Left => async () => {
  try {
    const res = await StartedContainer.value1.stop();
    return Right(StoppedContainer(res));
  } catch (e) {
    return Left(e.message);
  };
};

