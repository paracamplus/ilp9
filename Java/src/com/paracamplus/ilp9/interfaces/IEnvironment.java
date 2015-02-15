package com.paracamplus.ilp9.interfaces;

public interface IEnvironment<K,V,T extends Throwable> {
	/** is the key present in the environment ? */
	boolean isPresent(K key);
	IEnvironment<K,V,T> extend(K key, V value);
	K getKey() throws T;
	V getValue(K key) throws T;
    void update(K key, V value) throws T;
	// Low level interface:
	boolean isEmpty();
	IEnvironment<K,V,T> getNext() throws T;
}
