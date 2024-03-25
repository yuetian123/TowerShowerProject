using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class BaseClass<T> where T:new()
{
    public static T Instance;

    public T GetInstance()
    {
        if (Instance == null)
        {
            Instance = new T();
        }
        return Instance;
    }
}
