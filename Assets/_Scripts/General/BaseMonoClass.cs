﻿using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class BaseMonoClass<T> : MonoBehaviour where T : MonoBehaviour
{
    public static T Instance;

    protected virtual void Awake()
    {
        if (Instance == null)
        {
            Instance = this as T;
        }
    }
}
