import { DockerComposeEnvironment, PullPolicy, StartedDockerComposeEnvironment, Wait } from "testcontainers"

const cloneDockerComposeEnvironment = (dce) => {
  const clone = new DockerComposeEnvironment("", "");
  Object.assign(clone, dce);
  return clone;
};

export const mkComposeEnvironmentImpl = (composeFilePath, composeFiles) => {
  return new DockerComposeEnvironment(composeFilePath, composeFiles);
}

export const setEnvironmentFileImpl = (dce, environmentFile) => {
  const clone = cloneDockerComposeEnvironment(dce);
  clone.withEnvironmentFile(environmentFile);
  return clone;
};

export const setProfilesImpl = (dce, profiles) => {
  const clone = cloneDockerComposeEnvironment(dce);
  clone.withProfiles(...profiles);
  return clone;
};

export const setEnvironmentImpl = (dce, environment) => {
  const clone = cloneDockerComposeEnvironment(dce);
  const toJs = environment.reduce((acc, el) => {
    acc[el.key] = el.value;
    return acc;
  }, {});

  clone.withEnvironment(toJs);
  return clone;
};

/**
 * @param {DockerComposeEnvironment} dce
 */
export const environmentUpImpl = async (dce, services, Left, Right) => {
  try {
    if (services === undefined || services.length < 1) {
      return Right(await dce.up());
    } else {
      return Right(await dce.up(services));
    }
  } catch (e) {
    return Left(e.message);
  }
};

export const setNoRecreateImpl = (dce) => {
  const clone = cloneDockerComposeEnvironment(dce);
  clone.withNoRecreate();
  return clone;
};

export const setWaitStrategyImpl = (dce, container, waitStrategies) => {
  const clone = cloneDockerComposeEnvironment(dce);
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
  });

  clone.withWaitStrategy(container, Wait.forAll(toJs));
  return clone;
};

export const setRebuildImpl = (dce) => {
  const clone = cloneDockerComposeEnvironment(dce);
  clone.withBuild();
  return clone;
};

export const setPullPolicyImpl = (dce, pullPolicy) => {
  const clone = cloneDockerComposeEnvironment(dce);
  if (pullPolicy.constructor.name == "AlwaysPull") {
    clone.withPullPolicy(PullPolicy.alwaysPull());
  } else if (pullPolicy.constructor.name == "DefaultPolicy") {
    clone.withPullPolicy(PullPolicy.defaultPolicy());
  }

  return clone;
};

/**
 * @param {StartedDockerComposeEnvironment} dce
 */
export const getContainerImpl = (dce, container, Left, Right) => {
  try {
    return Right(dce.getContainer(container));
  } catch (e) {
    return Left(e.message);
  }
};

/**
 * @param {StartedDockerComposeEnvironment} dce
 */

export const environmentDownImpl = async (dce, Left, Right) => {
  try {
    const res = await dce.down();
    return Right(res);
  }
  catch (e) {
    return Left(e.message);
  };
};
