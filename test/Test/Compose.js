import { getContainerRuntimeClient } from "testcontainers";

export const composeVersionImpl = async () => {
  const runtime = await getContainerRuntimeClient();
  return runtime.info.compose.version;
};

