package com.paracamplus.ilp9.tools;

import java.util.List;

import com.paracamplus.ilp9.interfaces.IAST;

interface IASTlink<K, V> extends IAST {
	K getKey();
	V getValue();
}

public interface IEnvironment<K,V> extends List<IASTlink<K,V>> {
	
	/** is the key present in the environment ? */
	boolean isPresent(K key);
	IEnvironment<K,V> extend(K key, V value);
	void update(K key, V value);
	V getValue(K key);
	// Low level interface:
	boolean isEmpty();
	IASTlink<K,V> getFirstLink();
	IEnvironment<K,V> getOtherLinks();
}
