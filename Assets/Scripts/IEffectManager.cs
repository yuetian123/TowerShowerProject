using System.Collections.Generic;
using UnityEngine;

public class IEffectManager<T, T2> : MonoSingleton<T> where T : MonoBehaviour
{
    public List<T2> LookAtCameraList = new List<T2>();

    public static void Add(T2 effect)
    {
        var inst = Instance as IEffectManager<T, T2>;
        if (inst == null)
        {
            return;
        }

        if (inst.LookAtCameraList.Count == 0)
        {
            inst.enabled = true;
        }

        inst.LookAtCameraList.Add(effect);
    }

    public static void Remove(T2 effect)
    {
        var inst = Instance as IEffectManager<T, T2>;
        if (inst == null)
        {
            return;
        }

        inst.LookAtCameraList.Remove(effect);
        if (inst.LookAtCameraList.Count == 0)
        {
            inst.enabled = false;
        }
    }
}