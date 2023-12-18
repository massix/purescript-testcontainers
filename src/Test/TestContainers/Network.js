import { Network, StartedNetwork } from 'testcontainers';

export const mkNetworkImpl = Constructor => {
  return Constructor(new Network());
};

export const startNetworkImpl = async(stoppedNetwork) => {
  return await stoppedNetwork.start();
};

/**
 * @param {StartedNetwork} network
 */
export const getIdImpl = network => {
  return network.getId();
};

/**
 * @param {StartedNetwork} network
 */
export const getNameImpl = network => {
  return network.getName();
};

